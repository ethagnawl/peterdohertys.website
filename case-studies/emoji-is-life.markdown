---
title: Bleacher Report - Emoji Is Life
---

# TODO
- annotate code sample
- benchmark reduce vs. r/reduce
- call out possibility of using [reduce-kv](https://clojuredocs.org/clojure.core/reduce-kv) in reduce-emojis
- call out possibility of korma to translate funky table names (Series, seriesId, etc.)

# Introduction
I was recently tasked with building a back-end service which was used to
facilitate the creation of Bleacher Report's [Emoji Is
Life](http://thelab.bleacherreport.com/emoji-is-life/) story - a high-level
summary of the NBA playoffs as told through emoji.

The service was used during the editorial phase of the story's creation to
determine which emoji were being used most frequently when people were tweeting
during games' key moments.

A separate service, using Twitter's Streaming APIs, was responsible for
compiling Tweets sent out during each playoff game which met a number of
qualifications (@mentioned either/both teams, contained at least one emoji,
etc.). My service was responsible for making that data available and digestible
to editors.

# Overview
At a high-level, the service was built using Clojure, Docker, PostgreSQL and
deployed to Heroku.

## Docker
Docker and Docker Compose made standing-up the application for development and
testing simple and easy. Once the Dockerfile/docker-compose.yml config files
had been authored, everything _just worked_. Seeing the application come to
life with a single command, on a machine without any of the development
dependencies installed, was (in my humble opinion) a seminal moment.

## Service
The service itself was a Hypermedia API built using Ring, which made it
possible for editors to explore the different resources
(series, games, moments, etc.) in the hopes of uncovering data points which
would be useful when writing their story. (e.g. 3700 people authored tweets
using the 😳 emoji after Waiters elbowed Ginobili during the Thunder's win over
the Spurs in game 2 of the Western Conference semifinals)

### PostgreSQL/Korma
We used the Korma library to interface with PostgreSQL and it was a pleasure to
work with. Korma is a DSL that translates Clojure code into SQL statements -
and does helpful things like prevent SQL injection when using dynamic values
in queries. It requires you to write a bit more boilerplate than using an
opinionated ORM, but it is more flexible and composable.

For instance, here's the entity definition and series query function from
`bleacher-report-emoji-api.db`:

    (defdb db (postgres {...}))

    (declare series game emoji)

    (defentity series
       (table :Series)
       (has-many game {:fk :seriesId}))

    ;; other entities elided

    (defn query-series-plural []
      (select series))

    (defn query-series-singular [id]
      (let [series-id (read-string id)]
        (first (select series
          (where {:id series-id})
          (with game)))))

### Data Transformation
In order to make the game/moment data digestible for editors, the results of
the relevant SQL queries were run through a transformation function in order to
reduce a list of emoji IDs and team IDs into a map containing the top 10 emojis
used by each team and both teams. (e.g.  `{"both" [...], "ss" [...], "ot" [...]}`)

The data transformation function is as follows:

    (defn reduce-emojis [{:keys [winner-id loser-id emojis]}]
      "transforms {:emojis [{:emojiId 100 :teamId "cc"}
                            {:emojiId 200 :teamId "dd"}
                            {:emojiId 200 :teamId "dd"}
                            {:emojiId 300 :teamId "both"}]
                   :loser-id 2
                   :winner-id 1}
       into {"cc" [{:emoji a :emoji-id 100 :count 1}
                   {:emoji c :emoji-id 300 :count 1}]
             "dd" [{:emoji b :emoji-id 200 :count 2}
                   {:emoji c :emoji-id 300 :count 1}]
             "both" [{:emoji a :emoji-id 100 :count 1}
                     {:emoji b :emoji-id 200 :count 2}
                     {:emoji c :emoji-id 300 :count 1}]}"

      (let [emojis' (r/reduce
                      (fn [memo member]
                        (let [emoji-id (:emojiId member)
                              team-id (:teamId member)
                              get-new-count (fn [id]
                                              (let [path [id emoji-id]
                                                    old-count (if-let [count' (get-in memo path)]
                                                                count'
                                                                0)
                                                    new-count (inc old-count)]
                                                new-count))
                              new-both-count (get-new-count :both)
                              new-winner-count (get-new-count winner-id)
                              new-loser-count (get-new-count loser-id)
                              new-team-count (get-new-count team-id)]
                          (if (= team-id "both")
                            (-> memo
                                (assoc-in [winner-id emoji-id] new-winner-count)
                                (assoc-in [loser-id emoji-id] new-loser-count)
                                (assoc-in [:both emoji-id] new-both-count))
                            (-> memo
                              (assoc-in [team-id emoji-id] new-team-count)
                              (assoc-in [:both emoji-id] new-both-count)))))
                      {:both {}
                       winner-id {}
                       loser-id {}}
                      emojis)
            emojis'' (for [[team-id emojis] emojis']
                       {team-id (->>
                                  (for [[emoji-id count] emojis]
                                    {:emoji-id emoji-id
                                    :emoji (get emoji-map emoji-id)
                                    :count count})
                                  (sort-by :count >)
                                  (take 10)
                                  (vec))})
            emojis''' (into {} emojis'')]
        emojis'''))

While this transformation function worked well, it's certainly not as efficient
as it could have been (I haven't had a chance to revisit the implementation,
but I believe the entire transformation could be achieved in a single-pass). I
was also working under the mistaken impression that `r/reduce` was automatically
parallelized, but that turns out not to be the case*.

\* `r/fold`, amongst other functions in the Reducers library, are automatically parallelized, when doing so is efficient. [This](https://adambard.com/blog/clojure-reducers-for-mortals/) post provides a nice, high-level overview of the Reducers library. Also, be sure to check out [the official Reducers docs](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core.reducers).


# Lessons Learned

### 1.)

# Summary
Despite the issues mentioned above, the installation worked flawlessly while it was deployed and was a lot of fun to work on. I've worked on a few "physical" projects and they're a welcomed change of pace from my day-to-day work doing web/mobile development - it's surprisingly satisfying to see people interact with your software in the real-world.

# Collaborators
- Kate Strassman
- Mike Fey

# Resources
- [Docker Compose](https://docs.docker.com/compose/)
- [Improving your Clojure code with core.reducers](https://adambard.com/blog/clojure-reducers-for-mortals/)
- [Korma](https://github.com/korma/Korma)
- [Midje](https://github.com/marick/Midje)
- [Reducers](http://clojure.org/reference/reducers)
