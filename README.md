
# Laboratory work 3

Implementing a Message Broker.

## General Info

This laboratory work focuses on developing a message broker with support for multiple topics. The message broker is a dedicated async TCP server written in Erlang. The second laboratory work acts as a Publisher.

#### Structure of a message 

```erlang
    #{
        type => message,
        param => #{
            pubid => PubId,
            topic => Topic,
            message => Tweet
        }
    }

```
In the example above, there is a message that sends a tweet.

Communication between Publisher, Message Broker and Client happens through serialized Erlang maps, each containing a *Type* field. In this laboratory work, there are 10 types of messages.

#### Persistence
All messages are stored into a database, thus assuring durability. In case a client disconnects, no messages will be lost. Whenever a client receives a message, they send an ack in order to receive the next message.


### Supervision Tree

![Supervisor Diagram](https://user-images.githubusercontent.com/72708199/123850925-4040bf80-d923-11eb-8eb5-400200aea3bb.png)

### Publisher Messages Diagram

![Publisher Diagram](https://user-images.githubusercontent.com/72708199/123850888-374fee00-d923-11eb-9439-be51890182d6.png)

### Client Messages Diagram

![Client Diagram](https://user-images.githubusercontent.com/72708199/123850840-2b642c00-d923-11eb-9f5a-3d535b9f39eb.png)


## Technologies

The project is created in Erlang with:
- rebar3
- shotgun library
- jcx library

### Screencast
[Screencast](https://utm-my.sharepoint.com/:v:/g/personal/daniela_palamari_isa_utm_md/Ec13X4ki4QJHt0f_vA51YJYBZ2r3qcW7fWDWVYbZzzH-NQ?e=lLpMWp)
