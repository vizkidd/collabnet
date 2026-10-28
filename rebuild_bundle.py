import os
import re
import json

base_dir = "./"

helper_scripts = [
    "GScholarLENS-DOI2Data.R",
    "GScholarLENS-ORCID2Data.R",
    "GScholarLENS-SCOPUS2Data.R",
    "GScholarLENS-ProcessJCR.R",
    "GScholarLENS-Data2GLENS.R",
    "GScholarLENS-PlotGLENS.R",
    "GScholarLENS-Graph.R",
    "GScholarLENS-helpers.R",
    "GScholarLENS-Fixes.R",
    "GScholarLENS-MCP.R"
]
app_files = ["ui.R", "server.R", "app.R"]
all_files = helper_scripts + app_files

worker_js_src = os.path.join(base_dir, "www", "js", "collabnet-worker-pool.js")
worker_js_content = ""
if os.path.exists(worker_js_src):
    with open(worker_js_src, "r", encoding="utf-8") as f:
        worker_js_content = f.read()

combined_lines = []
for fname in all_files:
    fpath = os.path.join(base_dir, fname)
    with open(fpath, "r", encoding="utf-8") as f:
        for line in f:
            # Strip source(...) calls
            if re.search(r"^\s*source\s*\(", line):
                continue
            # # Strip require(httr2) to prevent rlang version conflict in WebR
            # if re.search(r"require\s*\(\s*httr2\s*\)", line):
            #     continue
            # if 'tags$script(src = "js/collabnet-worker-pool.js")' in line:
            #     if worker_js_content:
            #         combined_lines.append(f'    tags$script(HTML({json.dumps(worker_js_content)})),\n')
            #         continue
            combined_lines.append(line)

combined_text = "".join(combined_lines)

# Write to staging_app/app.R
staging_app_r = os.path.join(base_dir, "staging_app", "app.R")
os.makedirs(os.path.dirname(staging_app_r), exist_ok=True)
with open(staging_app_r, "w", encoding="utf-8") as f:
    f.write(combined_text)
print(f"Wrote {len(combined_text)} chars to {staging_app_r}")

# Update wasm_build/app.json
app_json_path = os.path.join(base_dir, "wasm_build", "app.json")
with open(app_json_path, "r", encoding="utf-8") as f:
    app_data = json.load(f)

updated = False
for item in app_data:
    if item.get("name") == "app.R":
        item["content"] = combined_text
        updated = True
        break

if not updated:
    app_data.insert(0, {"name": "app.R", "content": combined_text})

# Sync www assets (js, css, fonts, images) to staging_app and wasm_build
import shutil

www_dir = os.path.join(base_dir, "www")
if os.path.exists(www_dir):
    for root, dirs, files in os.walk(www_dir):
        for f in files:
            src_file = os.path.join(root, f)
            rel_path = os.path.relpath(src_file, www_dir)
            for dest_root in [os.path.join(base_dir, "staging_app"), os.path.join(base_dir, "wasm_build")]:
                dest_file = os.path.join(dest_root, rel_path)
                os.makedirs(os.path.dirname(dest_file), exist_ok=True)
                shutil.copy2(src_file, dest_file)
            
            # Also register text assets into app.json virtual filesystem
            if f.endswith((".js", ".css", ".json")):
                with open(src_file, "r", encoding="utf-8") as sf:
                    content = sf.read()
                asset_name = rel_path.replace("\\", "/")
                found = False
                for item in app_data:
                    if item.get("name") in [asset_name, f"www/{asset_name}"]:
                        item["content"] = content
                        found = True
                        break
                if not found:
                    app_data.append({"name": asset_name, "content": content})

with open(app_json_path, "w", encoding="utf-8") as f:
    json.dump(app_data, f)
print(f"Updated {app_json_path} and synced all www/ assets successfully")
