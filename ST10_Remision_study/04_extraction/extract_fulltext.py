#!/usr/bin/env python3
"""
Study: ST10_Remision_study
Script: extract_fulltext.py
Author: Nichodemus Werre Amollo
Date: 2026-06-12
Purpose: Print abstracts and outcome-bearing sentences from saved PubMed Central
         full-text JSON (get_full_text_article MCP tool) to support data extraction.
Usage:   python extract_fulltext.py <fulltext.json> [pmid ...]
"""
import json, re, sys, textwrap

KEY = re.compile(
    r'(remission|medication[- ]?free|withdraw|discontinu|HbA1c|glycated|'
    r'random|allocat|primary outcome|control group|intervention group|'
    r'\bn\s*=\s*\d|achiev|fasting plasma|FPG)', re.I)

def sentences(text):
    return re.split(r'(?<=[.!?])\s+(?=[A-Z0-9])', text)

def main(args):
    fp = args[0]
    want = set(args[1:])
    d = json.load(open(fp, encoding="utf8"))
    for a in d.get("articles", []):
        pmid = a.get("identifiers", {}).get("pmid", "?")
        if want and pmid not in want:
            continue
        print("\n" + "#"*100)
        print(f"PMID {pmid} | {a.get('title','')[:95]}")
        print("="*40 + " ABSTRACT " + "="*40)
        print(textwrap.fill(a.get("abstract","") or "", 118))
        ft = a.get("full_text","") or ""
        print("="*38 + " KEY SENTENCES " + "="*38)
        for s in sentences(ft):
            s = s.strip()
            if 12 < len(s) < 400 and KEY.search(s):
                print("•", textwrap.fill(s, 116, subsequent_indent="  "))

if __name__ == "__main__":
    main(sys.argv[1:])
