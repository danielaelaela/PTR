# Laboratory work 2
Loading streaming data into a database.

## General Info

This laboratory work is a system that processes real Twitter API data using SSE's (Server Sent Events), computes the Sentiment Scores and the Engagement Ratios of the tweets extracted, merges all information and loads it into a database. 

### Tweet Route
![Tweet Route](https://user-images.githubusercontent.com/72708199/115045344-19a3e880-9edf-11eb-8028-3830b8d8f577.png)

The **Filter** receives SSE's from the **Collectors**, filters and adds an ID to tweets, then sends the *{ID, Tweet}* maps to the Worker Pools and the Aggregator. 

The **Aggregator** takes care of merging the resulting Sentiment Scores, Engagement Ratios and ID'd Tweets together, then sends the *{ID, SS, ER, Tweet}* maps to the **Batcher**.

The **Batcher** makes sure the data is sent to the database in batches of maximum size 128, or after each 0.2 seconds. 

The **Storage** creates the database, separates and stores the received data in:
```sql
Event (Id, TweetId, UserId, SS, ER).
User (UserId, User).
Tweet (TweetId, Tweet).
```
### Worker Pool Diagram

![Worker Pool Diagram (2)](https://user-images.githubusercontent.com/72708199/115049607-9769f300-9ee3-11eb-8597-4aeaf7ef1ccf.png)


### Supervision Tree of SuperSupervisor
![Supervision Tree SupSup (2)](https://user-images.githubusercontent.com/72708199/115050186-2bd45580-9ee4-11eb-9a18-9f35cebbb6ae.png)


### Supervision Tree of PoolSupervisor
![Supervision Tree PoolSup](https://user-images.githubusercontent.com/72708199/115049907-e3b53300-9ee3-11eb-965a-d204e6957047.png)

## Technologies

The project is created in Erlang with:
- rebar3
- shotgun library
- jcx library

### Screencast
[Screencast](https://utm-my.sharepoint.com/:v:/g/personal/daniela_palamari_isa_utm_md/EfPgRIcziVVIljE-Mpddah0BfmWGrr7ntm625IOZj_8x5Q?e=PG8qpF)
