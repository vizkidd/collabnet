import os
import re
import json

base_dir = "/home/linuxusb/Downloads/collabnet-main-mcp/collabnet-main"

helper_scripts = [
    "GScholarLENS-DOI2Data.R",
    "GScholarLENS-ORCID2Data.R",
    "GScholarLENS-SCOPUS2Data.R",
    "GScholarLENS-ProcessJCR.R",
    "GScholarLENS-Data2GLENS.R",
    "GScholarLENS-PlotGLENS.R",
    "GScholarLENS-Graph.R",
    "GScholarLENS-helpers.R",
    "GScholarLENS-Fixes.R"
]
app_files = ["ui.R", "server.R", "app.R"]
all_files = helper_scripts + app_files

combined_lines = []
for fname in all_files:
    fpath = os.path.join(base_dir, fname)
    with open(fpath, "r", encoding="utf-8") as f:
        for line in f:
            # Strip source(...) calls
            if re.search(r"^\s*source\s*\(", line):
                continue
            # Strip require(httr2) to prevent rlang version conflict in WebR
            if re.search(r"require\s*\(\s*httr2\s*\)", line):
                continue
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

with open(app_json_path, "w", encoding="utf-8") as f:
    json.dump(app_data, f)
print(f"Updated {app_json_path} successfully")
