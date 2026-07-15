---
post_type: blog-post
published: true
published_on: 4/29/2026
title: Full-Text Search with DuckDB
tags: duckdb
---

## Overview

This is a follow-up to my first post about DuckDB: [A Dab of DuckDB](https://peterdohertys.website/blog-posts/dab-of-duck.html). If you're new to DuckDB, you may want to start there.

The basic DuckDB workflow of making a data source quickly and easily discoverable is incredibly powerful ... but there are limits. Some use cases, like searching the contents of historical publications or a tranche of emails would be constrained by basic text queries. As mentioned in my first post, I'm interested in exploring some of the more powerful DuckDB features and in this post I'll be focused on full-text search (FTS). I have a decent amount of experience using other FTS solutions, like Elasticsearch and Postgres (both with the in-built options and extensions like pgvector and pg_search). So, in this post I will take you through a quick tour of the current state of FTS in DuckDB.


### An abbreviated FTS primer

A full FTS tutorial is outside the scope of this post and if you're interested in learning more [the Postgres docs](https://www.postgresql.org/docs/current/textsearch.html) are a worthwhile read.

FTS allows for more comprehensive and configurable queries than what can be achieved using SQL operators like `=`, `ilike` or regexen. Query scores can also be tuned using algorithms, like [Okapi BM25](https://en.wikipedia.org/wiki/Okapi_BM25), which is what DuckDB offers.

Index options:

    - Stemming: reduces words to a common root and handles some forms of inflection (walk, walks, walked, walking, etc.) but there are gaps for unconventional forms (e.g. mice and mouse)
    - Stop words: removal of common "stop words" like "the", "and" and "of" whose presence may skew results
    - Strip accents: normalize "á", "ä", and "a"

Query functions:

    - Okapi BM25 parameters

        - k₁: term frequency (are more instances meaningful?)
        - b: length normalization (is a longer document more meaningful?)

The features mentioned above are all present in DuckDB's FTS extension but this only begins to scratch the surface of what is possible with FTS -- especially when using some of the more fully-featured engines. The DuckDB feature set is a good start, though, and I'm sure functionality will be added over time and/or new extensions will be released. I imagine functions for phrase queries, vectors and support for pluggable synonym dictionaries are features contributors are already thinking about.

One example of where the current DuckDB feature-set fell short during my experiments was its lack of any way to highlight _where_ query terms matched in the source data. Postgres offers `ts_headline` to address this but I found myself having to search query results for matches using tmux (a string of keystrokes I can never remember ...) or by piping query results to grep and that was a bit frustrating.

While working on this post, I also came across [the Snowball project](https://snowballstem.org/) which is "a small string processing language for creating stemming algorithms for use in Information Retrieval, plus a collection of stemming algorithms implemented using it". My understanding is that it's the basis for stemming in most databases and client libraries. [The Python snowballstemmer library](https://pypi.org/project/snowballstemmer/) can be used to quickly debug unexpected stemming issues. For example, why certain word forms aren't being matched.

```

# stemmer.py
# Usage: uv run stemmer.py
# /// script
# requires-python = "==3.13"
# dependencies = [
# "snowballstemmer==3.0.1",
# ]
# ///

from snowballstemmer import stemmer

en = stemmer("english")

print(en.stemWord("run"))     # -> run
print(en.stemWord("running")) # -> run

print(en.stemWord("mouse"))   # -> mous
print(en.stemWord("mice"))    # -> mice
```


## Setting Up

Full-text search isn't something DuckDB provides out-of-the-box but it is readily available via the [Full-Text Search Extension](https://duckdb.org/docs/current/core_extensions/full_text_search).

Assuming you've already got DuckDB installed, installing the fts extension is as simple as starting a new session and running:

```
INSTALL fts;
LOAD fts;
```

## Digging In

Let's assume you have a multi-GB trove of emails and you want to search their contents to see what politicians, business leaders and celebrities are talking with each other about. In my corpus, there are 13,010 emails with the .eml extension which have a smattering of mime-types. DuckDB can't import these natively, so we'll have to do some pre-processing before we can create our database, index it and start querying. You should be able to find a tranche of emails like this one on archive.org, ddosecrets.org or even justice.gov but that's left as an exercise for the reader. If you want to follow along, any collection of .emls should suffice.


### Pre-Processing Files

I'm going to use Python to do the processing of the raw files. YMMV, but when it comes to Python tooling, I don't think any solution comes close to being as simple and efficient as [uv](https://docs.astral.sh/uv/) and that's what I'll be using. (I've given a few lightning talks on uv and really need to dedicate a blog post to it.)

The pre-processing workflow is admittedly quick and dirty and will readily discard emails which can't be cleanly parsed:

    - load the email file
    - try to parse out its body contents
    - parse out useful headers and other metadata to help flag marketing and transactional emails
    - if successful, dump JSON to a file

```
# preprocess-emails.py
# Usage: uv run preprocess-emails.py
# /// script
# requires-python = "==3.13"
# dependencies = [
# "beautifulsoup4==4.14.3",
# ]
# ///

import email
import json
from bs4 import BeautifulSoup
from email import policy
from pathlib import Path


def html_to_text(html):
    soup = BeautifulSoup(html, "html.parser")
    for tag in soup(["script", "style", "head", "title", "meta"]):
        tag.decompose()
    return soup.get_text(" ", strip=True)


def extract_body(msg):
    try:
        for kind in ("plain", "html"):
            part = msg.get_body(preferencelist=(kind,))
            if part is None:
                continue
            try:
                content = part.get_content()
            except Exception:
                continue
            if not (content and content.strip()):
                continue
            return html_to_text(content) if kind == "html" else content
        return None
    except Exception as e:
        print(f"Couldn't parse body: {e}")
        return None


for path in Path(".").glob("*.eml"):
    try:
        with open(path, "rb") as f:
            msg = email.message_from_binary_file(f, policy=policy.default)
        body = extract_body(msg)

        if not body:
            print(f"no body found for {f}")
            continue

        row = {
            "body": body,
            "date": str(msg["date"]),
            "file": path.name,
            "from": str(msg["from"]),
            "subject": str(msg["subject"]),
            "to": str(msg["to"]),
            "list_unsubscribe": str(msg.get("List-Unsubscribe", "")),
            "list_id": str(msg.get("List-Id", "")),
            "precedence": str(msg.get("Precedence", "")),
            "auto_submitted": str(msg.get("Auto-Submitted", "")),
            "x_mailer": str(msg.get("X-Mailer", "")),
            "return_path": str(msg.get("Return-Path", "")),
        }

        with open(f"{path}.json", "w") as f:
            f.write(json.dumps(row))

    except Exception as e:
        print(f"error parsing {f}: {e}")
```


### Regularly Scheduled Programming

#### Import JSON and Populate DB
```
CREATE TABLE emails AS SELECT * FROM read_json('*.eml.json');
```

I *love* that DuckDB shows the progress of the import and how much RAM is being used.

#### Create and populate ID column

There might be a way to do this during import and you could certainly insert IDs from the pre-processing loop but I'd forgotten to do so and I'll leave this sequence for posterity.

```
ALTER TABLE emails ADD COLUMN id INTEGER;
UPDATE emails SET id = rowid;
```


#### Create FTS Index

You can index one or more columns and there are optional parameters to control the stemmer, stop words, etc. See the docs [here](https://duckdb.org/docs/current/core_extensions/full_text_search#pragma-create_fts_index).

```
PRAGMA create_fts_index('emails', 'id', 'subject', 'body');
```

#### Start Digging!

There are various parameters which can be used to refine or broaden your queries as needed. See the docs [here](https://duckdb.org/docs/current/core_extensions/full_text_search#match_bm25-function).

Basic query using default parameters and an attempt at excluding transactional emails and mailing lists:
```
SELECT id, body, fts_main_emails.match_bm25(id, 'talk') AS score
FROM emails
WHERE list_unsubscribe = ''
    AND precedence NOT IN ('bulk', 'list', 'junk')
    AND score IS NOT NULL
ORDER BY score DESC;
-- yields results for: "Talking", "talks", "talked", etc.
```

Use `conjunctive` parameter to require that all terms match:

```
SELECT id, body, fts_main_emails.match_bm25(id, 'detective trial', conjunctive := 1) AS score
FROM emails
WHERE list_unsubscribe = ''
    AND precedence NOT IN ('bulk', 'list', 'junk')
    AND score IS NOT NULL
ORDER BY score DESC;
-- yields only results matching *both* search terms
```

Use the Okapi `k₁` and `b` parameters to weight term frequency and document length:

b:

```
-- b = 0: document length is effectively ignored
SELECT subject, "from", length(body) AS body_len,
       fts_main_emails.match_bm25(id, 'delivery', b := 0.0) AS score
FROM emails
WHERE score IS NOT NULL
ORDER BY score DESC
LIMIT 1;

┌────────────────────────────────────────┬─────────────────────────────────────────┬──────────┐
│                subject                 │                  from                   │ body_len │
│                varchar                 │                 varchar                 │  int64   │
├────────────────────────────────────────┼─────────────────────────────────────────┼──────────┤
│ Our Best Deal of the Year: Save 50% fo │ The New York Times <nytimes@email.newyo │     4121 │
│ r 26 Weeks on a Times Subscription. Sa │ rktimes.com>                            │          │
│ le Ends 12/2                           │                                         │          │
└────────────────────────────────────────┴─────────────────────────────────────────┴──────────┘


-- b = 1: long documents (e.g. newsletter and articles) are penalized
SELECT subject, "from", length(body) AS body_len,
       fts_main_emails.match_bm25(id, 'delivery', b := 1.0) AS score
FROM emails
WHERE score IS NOT NULL
ORDER BY score DESC
LIMIT 1;

┌────────────────────────┬───────────────────────────────────────┬──────────┐
│        subject         │                 from                  │ body_len │
│        varchar         │                varchar                │  int64   │
├────────────────────────┼───────────────────────────────────────┼──────────┤
│  DELIVERY for Member   │ Sapply <newsletter@sapplysamples.com> │      197 │
└────────────────────────┴───────────────────────────────────────┴──────────┘
```

k₁:

I wasn't able to find a good example of how `k₁` would be useful using my existing corpus, so I created two synthetic emails/rows which emphasize how the k₁ parameter changes its score for word frequency. One email mentions "budget" once and the other mentions "budget" repeatedly, as it's the focus of the email. When k₁ is low, the scores are closer (they actually match in this case but that wouldn't be true across a real data set) and when k₁ is high, the email which is actually about "the budget" is scored higher.

```
SELECT file, subject,
    length(regexp_extract_all(lower(body), 'budget')) AS instances,
    fts_main_emails.match_bm25(id, 'budget', k := 0.3) AS k_low,
    fts_main_emails.match_bm25(id, 'budget', k := 3.0) AS k_high
FROM emails
WHERE file LIKE 'demo-%'
ORDER BY k_high DESC;

┌──────────────────────────┬───────────┬────────────────────┬────────────────────┐
│           file           │ instances │       k_low        │       k_high       │
│         varchar          │   int64   │       double       │       double       │
├──────────────────────────┼───────────┼────────────────────┼────────────────────┤
│ demo-budget-focused.eml  │         8 │ 1.5647278150907307 │  5.693932504469362 │
│ demo-passing-mention.eml │         1 │ 1.5647278150907307 │ 3.2223555634031062 │
└──────────────────────────┴───────────┴────────────────────┴────────────────────┘
```

Using a low k₁, the scoring strategy is effectively, "did this term appear at all?", while with a high k₁, repeated instances affect the ranking.

### Cleaning Up

The index can be deleted using:

```
PRAGMA drop_fts_index('emails');
```

## Summary

DuckDB's FTS feature set is not as feature-complete as those in Postgres or Elasticsearch. However, it's still quite powerful and likely _good enough_ for most exploratory use cases. If you determine that you need a more complex solution, it should be easy enough to dump your DuckDB and import it into Postgres or Elasticsearch. The ease and speed with which it can be spun up against (almost) any data source makes it very compelling and something I will reach for when I start using DuckDB for Real Work.

I would like to continue this series on DuckDB and my next post may investigate the current state of vector search. Stay tuned.

## Updates
- 7/15/2026 - added duckdb post series links
