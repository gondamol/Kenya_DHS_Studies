#!/usr/bin/env python3
"""
Study: ST10_Remision_study
Script: screen_pubmed.py
Author: Nichodemus Werre Amollo
Date: 2026-06-12
Purpose: Process saved PubMed metadata JSON (from the get_article_metadata MCP tool)
         into a compact title/abstract screening view, flagging World Bank LMIC
         country mentions and RCT design to support eligibility screening.

Usage:   python screen_pubmed.py <metadata1.json> [<metadata2.json> ...]
         (Pass the tool-result files saved by get_article_metadata.)
"""
import json, sys, re

# World Bank FY2026 low- and middle-income economies (working list; extend as needed).
LMIC = [
    "Afghanistan","Bangladesh","Benin","Bolivia","Brazil","Burkina Faso","Cambodia",
    "Cameroon","China","Colombia","Cote d'Ivoire","Ivory Coast","Egypt","Ethiopia","Ghana",
    "India","Indonesia","Iran","Iraq","Kenya","Malawi","Mexico","Morocco","Mozambique",
    "Myanmar","Nepal","Nigeria","Pakistan","Peru","Philippines","Rwanda","Senegal",
    "South Africa","Sri Lanka","Sudan","Tanzania","Thailand","Tunisia","Uganda","Ukraine",
    "Vietnam","Viet Nam","Zambia","Zimbabwe","Jordan","Lebanon","Algeria","Angola",
]
HIC = [
    "United Kingdom","England","Scotland","UK","USA","United States","Australia","Canada",
    "Germany","France","Italy","Spain","Netherlands","Sweden","Denmark","Norway","Finland",
    "Japan","Korea","Qatar","Kuwait","Saudi","United Arab Emirates","Singapore","Switzerland",
    "Austria","Belgium","Ireland","New Zealand","Israel","Taiwan","Hong Kong",
]

def country_hits(text, vocab):
    return sorted({c for c in vocab if re.search(r'\b'+re.escape(c)+r'\b', text, re.I)})

def main(files):
    seen = {}
    for fp in files:
        data = json.load(open(fp, encoding="utf8"))
        for a in data.get("articles", []):
            pmid = a.get("identifiers", {}).get("pmid", "?")
            seen[pmid] = a
    for pmid, a in seen.items():
        title = a.get("title", "") or ""
        abstract = a.get("abstract", "") or ""
        affs = " ".join(
            aff for au in a.get("authors", []) for aff in (au.get("affiliations") or [])
        )
        blob = " ".join([title, abstract, affs])
        lmic = country_hits(blob, LMIC)
        hic = country_hits(affs or blob, HIC)
        types = a.get("article_types", [])
        is_rct = ("Randomized Controlled Trial" in types) or bool(
            re.search(r'random', title + abstract, re.I))
        yr = a.get("publication_date", {}).get("year", "?")
        jrnl = a.get("journal", {}).get("iso_abbreviation", "")
        print("="*100)
        print(f"PMID {pmid} | {yr} | {jrnl} | RCT={is_rct}")
        print(f"LMIC_hits={lmic or '-'}  HIC_hits={hic or '-'}")
        print(f"TITLE: {title}")
        print(f"ABSTRACT: {abstract[:700]}")
    print("="*100)
    print(f"TOTAL unique records processed: {len(seen)}")

if __name__ == "__main__":
    main(sys.argv[1:])
