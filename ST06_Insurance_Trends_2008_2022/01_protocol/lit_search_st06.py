import requests, json, os

out_dir = os.path.dirname(os.path.abspath(__file__))

queries = [
    ("PubMed", "Kenya health insurance trend DHS"),
    ("PubMed", "Kenya NHIF coverage trends"),
    ("PubMed", "Kenya health insurance inequality equity DHS"),
    ("OpenAlex", "Kenya health insurance trends DHS 2008 2014 2022"),
    ("OpenAlex", "Kenya NHIF coverage trends longitudinal"),
    ("OpenAlex", "Kenya insurance coverage inequality concentration index DHS")
]

lines = ["# ST06 Literature Search Notes", "", "Search date: 2026-03-31", ""]

for db, q in queries:
    lines += [f"## Database: {db}", f"Query: `{q}`", ""]
    try:
        if db == "PubMed":
            esearch = requests.get(
                "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
                params={"db":"pubmed", "term":q, "retmode":"json", "retmax":20},
                timeout=30
            )
            esearch.raise_for_status()
            data = esearch.json()["esearchresult"]
            ids = data.get("idlist", [])
            count = data.get("count", "0")
            lines.append("Results count: " + count)
            if ids:
                summary = requests.get(
                    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi",
                    params={"db":"pubmed", "id":",".join(ids[:10]), "retmode":"json"},
                    timeout=30
                )
                summary.raise_for_status()
                sdat = summary.json()["result"]
                lines.append("Top records:")
                for pmid in ids[:10]:
                    rec = sdat.get(pmid, {})
                    title = rec.get("title", "")
                    pubdate = rec.get("pubdate", "")
                    source = rec.get("source", "")
                    lines.append("- PMID " + pmid + ": " + title + " (" + source + ", " + pubdate + ")")
            lines.append("")
        else:
            r = requests.get(
                "https://api.openalex.org/works",
                params={"search":q, "per-page":10, "filter":"from_publication_date:2015-01-01"},
                timeout=30
            )
            r.raise_for_status()
            data = r.json()
            count_val = data.get("meta",{}).get("count","NA")
            lines.append("Results count: " + str(count_val))
            lines.append("Top records:")
            for w in data.get("results", [])[:10]:
                title = w.get("display_name", "")
                year = w.get("publication_year", "")
                host = (w.get("primary_location", {}) or {}).get("source", {})
                journal = host.get("display_name", "") if host else ""
                doi = w.get("doi", "") or ""
                lines.append("- " + title + " (" + journal + ", " + year + ") DOI: " + doi)
            lines.append("")
    except Exception as e:
        lines.append("ERROR: " + str(e))
        lines.append("")

out_path = os.path.join(out_dir, "ST06_literature_search_raw.md")
with open(out_path, "w", encoding="utf-8") as f:
    f.write("\n".join(lines))

print("Wrote: " + out_path)
