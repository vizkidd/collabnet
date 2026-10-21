#!/usr/bin/env python3
"""
inject_packages.py

Injects all pre-downloaded WASM R packages from:
  wasm_build/shinylive/webr/packages/
into:
  wasm_build/shinylive/webr/library.data.gz
  wasm_build/shinylive/webr/library.js.metadata

This embeds all required R packages directly into the Emscripten Virtual File System (VFS)
mounted at /shinylive/library. When the browser opens CollabNET, WebR discovers the packages
already in .libPaths(), eliminating 60+ HTTP downloads and reducing cold boot time from
90+ seconds to under 5 seconds (full offline operation).
"""

import os
import sys
import json
import gzip
import tarfile
import shutil
import urllib.request

REPO_URL = "https://repo.r-wasm.org/bin/emscripten/contrib/4.6"
ESSENTIAL_PACKAGES = {
    "UpSetR": "1.4.1",
    "colourpicker": "1.3.0",
    "data.table": "1.18.6.1",
    "gridExtra": "2.3.1",
    "httr": "1.4.9",
    "miniUI": "0.1.2",
    "plotly": "4.12.1",
    "plyr": "1.8.9",
    "shinyjs": "2.1.1",
    "showtext": "0.9-8",
    "showtextdb": "3.0",
    "stringdist": "0.9.17",
    "sysfonts": "0.8.9",
    "visNetwork": "2.1.4"
}

def ensure_packages(pkg_dir):
    """Ensures all essential CollabNET packages exist in pkg_dir before injection."""
    os.makedirs(pkg_dir, exist_ok=True)
    for pkg, ver in ESSENTIAL_PACKAGES.items():
        tar_name = f"{pkg}_{ver}.tgz"
        pkg_subdir = os.path.join(pkg_dir, pkg)
        os.makedirs(pkg_subdir, exist_ok=True)
        target = os.path.join(pkg_subdir, tar_name)
        if not os.path.exists(target) or os.path.getsize(target) == 0:
            url = f"{REPO_URL}/{tar_name}"
            try:
                print(f"Downloading essential package {pkg} ({tar_name})...")
                req = urllib.request.Request(url, headers={"User-Agent": "R (4.6.0)"})
                with urllib.request.urlopen(req, timeout=15) as resp, open(target, "wb") as f:
                    f.write(resp.read())
                print(f"Successfully downloaded {pkg}.")
            except Exception as e:
                print(f"Notice: Could not download {pkg} from {url} ({e}); continuing with existing packages.")

def inject(build_dir="wasm_build"):
    webr_dir = os.path.join(build_dir, "shinylive", "webr")
    meta_path = os.path.join(webr_dir, "library.js.metadata")
    data_path = os.path.join(webr_dir, "library.data.gz")
    pkg_dir = os.path.join(webr_dir, "packages")

    if not os.path.exists(meta_path) or not os.path.exists(data_path):
        print(f"Error: {meta_path} or {data_path} not found.")
        return False

    # Auto-ensure all essential packages are downloaded
    ensure_packages(pkg_dir)

    print(f"Reading existing metadata from {meta_path}...")
    with open(meta_path, "r", encoding="utf-8") as f:
        meta = json.load(f)

    existing_files = meta.get("files", [])
    existing_pkgs = set()
    for entry in existing_files:
        parts = entry["filename"].strip("/").split("/")
        if parts:
            existing_pkgs.add(parts[0])

    print(f"Base packages currently in library.data.gz: {len(existing_pkgs)}")

    print(f"Decompressing {data_path}...")
    with gzip.open(data_path, "rb") as f:
        uncompressed_data = bytearray(f.read())

    current_offset = len(uncompressed_data)
    print(f"Current uncompressed VFS image size: {current_offset / (1024*1024):.2f} MB")

    # Find all .tgz packages
    tgz_files = []
    for root, dirs, files in os.walk(pkg_dir):
        for file in files:
            if file.endswith(".tgz"):
                tgz_files.append(os.path.join(root, file))

    tgz_files.sort()
    injected_count = 0
    injected_files_count = 0

    for tgz_path in tgz_files:
        pkg_name = os.path.basename(os.path.dirname(tgz_path))
        if pkg_name == "packages" or not pkg_name:
            pkg_name = os.path.basename(tgz_path).split("_")[0]
        if pkg_name in existing_pkgs:
            continue

        try:
            with tarfile.open(tgz_path, "r:gz") as tar:
                for member in tar.getmembers():
                    if member.isfile():
                        fobj = tar.extractfile(member)
                        if fobj is None:
                            continue
                        content = fobj.read()
                        start_pos = current_offset
                        uncompressed_data.extend(content)
                        end_pos = start_pos + len(content)
                        current_offset = end_pos

                        # Emscripten VFS filename format: /{member.name}
                        fn = "/" + member.name.lstrip("/")
                        existing_files.append({
                            "filename": fn,
                            "start": start_pos,
                            "end": end_pos
                        })
                        injected_files_count += 1
            existing_pkgs.add(pkg_name)
            injected_count += 1
        except Exception as e:
            print(f"Warning: Failed to extract {tgz_path}: {e}")

    print(f"Injected {injected_count} new packages ({injected_files_count} files).")
    print(f"New uncompressed VFS image size: {current_offset / (1024*1024):.2f} MB")

    # Update metadata
    meta["files"] = existing_files
    meta["remote_package_size"] = current_offset
    meta["gzip"] = True

    # Backup original metadata and data.gz
    backup_meta = meta_path + ".orig"
    backup_data = data_path + ".orig"
    if not os.path.exists(backup_meta):
        shutil.copy2(meta_path, backup_meta)
    if not os.path.exists(backup_data):
        shutil.copy2(data_path, backup_data)

    print(f"Saving updated {meta_path}...")
    with open(meta_path, "w", encoding="utf-8") as f:
        json.dump(meta, f, separators=(",", ":"))

    print(f"Compressing updated {data_path} (this may take a few seconds)...")
    with gzip.open(data_path, "wb", compresslevel=6) as f:
        f.write(uncompressed_data)

    new_gz_size = os.path.getsize(data_path) / (1024*1024)
    print(f"Done! New {data_path} size: {new_gz_size:.2f} MB. Packages successfully pre-baked!")
    return True

if __name__ == "__main__":
    bdir = sys.argv[1] if len(sys.argv) > 1 else "wasm_build"
    inject(bdir)
