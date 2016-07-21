---
title: Bleacher Report - Emoji Is Life
---

# TODO
- annotate code sample
- benchmark reduce vs. r/reduce
- call out possibility of using [reduce-kv](https://clojuredocs.org/clojure.core/reduce-kv) in reduce-emojis
- call out possibility of korma to translate funky table names (Series, seriesId, etc.)

# Introduction
I was recently tasked with building a back-end service for Bleacher Report's
[Emoji Is Life](http://thelab.bleacherreport.com/emoji-is-life/) story.

# Overview

### ...

# Lessons Learned/Suspicions Confirmed

### 1.)

# Summary
Despite the issues mentioned above, the installation worked flawlessly while it was deployed and was a lot of fun to work on. I've worked on a few "physical" projects and they're a welcomed change of pace from my day-to-day work doing web/mobile development - it's surprisingly satisfying to see people interact with your software in the real-world.

# Collaborators
- Kate Strassman
- Mike Fey

# Resources
- [Docker Compose](https://docs.docker.com/compose/)
- [Korma](https://github.com/korma/Korma)
- [Midje](https://github.com/marick/Midje)

# Code Sample

    (defn reduce-emojis [{:keys [winner-id loser-id emojis]}]
      "transforms {:emojis [{:emojiId 100 :teamId 1}
                            {:emojiId 200 :teamId 2}
                            {:emojiId 300 :teamId \"both\"}]
                   :loser-id 2
                   :winner-id 1}
       into {\"cc\" [{:emoji a :emoji-id 100 :count 1}
                     {:emoji c :emoji-id 300 :count 1}]
             \"dd\" [{:emoji b :emoji-id 200 :count 2}
                     {:emoji c :emoji-id 300 :count 1}]
             :both [{:emoji a :emoji-id 100 :count 1}
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
