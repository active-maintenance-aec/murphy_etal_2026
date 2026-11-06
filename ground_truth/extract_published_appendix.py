#!/usr/bin/env python3
"""Read the appendix's tables from its PDF into committed CSVs. Run once by hand; the PDF is not
redistributed here. Usage: extract_published_appendix.py <appendix.pdf>. Writes
ground_truth/published_appendix_values.csv and ground_truth/published_table_d16.csv."""
import csv, re, subprocess, sys
text = subprocess.run(["pdftotext", "-layout", sys.argv[1], "-"], capture_output=True, text=True).stdout
text = text.replace("\f", "\n").replace("−", "-")
TOKEN = re.compile(r"(?<![\w.])-?\d*\.?\d+\*?(?![\w.])")
rows = []
for m in re.finditer(r"Table ([CE]\.\d+): Predicting.*?\n(.*?)\* p <0\.05", text, re.S):
    body = m.group(2)
    body = body[body.index("Intercept"):]
    for i, tok in enumerate(TOKEN.findall(body), 1):
        rows.append({"table": m.group(1), "position": i, "value_paper": tok})
with open("ground_truth/published_appendix_values.csv", "w", newline="") as f:
    w = csv.DictWriter(f, ["table", "position", "value_paper"]); w.writeheader(); w.writerows(rows)
d16 = re.findall(r"^\s*(\d+)\s{2,}(.+?)\s{2,}((?:Pro|Anti) \w+)\s+(positive|negative|both)\s*$", text, re.M)
with open("ground_truth/published_table_d16.csv", "w", newline="") as f:
    w = csv.writer(f); w.writerow(["ad_id", "title", "valence", "emotion_valence"]); w.writerows(d16)
print(len(rows), "cells in", len({r["table"] for r in rows}), "tables;", len(d16), "D.16 rows")
