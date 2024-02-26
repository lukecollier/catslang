# Plunger

| :warning:        Experimental |
|-------------------------------|

_If fedsql is the question what is the answer?_

## Explaination

Catalyst solves a lot of problems, so why re-implement any of it?

Well it's JVM and has existed a while:
* Not very portable
* Often slow due to multiple graph traversal's

We could simply improve performance, aggregate multiple treversals and do a deep dive into performance. But that wouldn't solve the portability problem.

Why is portability important? For several use cases: For example analyzer should be able to operate **quickly** and **anywhere** for the sake of LSP's. We can't get adoption without first class developer support.

Designing the analyzer for these distinct use cases may hold several advantages which will be proved by this repo, furthermore the opportunity for an analyzer to integrate with our Habu services is a zero sum game. 

While you may expect that the multiple year development time of fedssql and it's contituant parts make it impossible to replace, theres a wealth of evidence that shows re-implementation (specifically from jvm based solutions) is not only _possible_ but *common*.

On top of the curiosity of what can be achieved it's also a chance for _me_ to learn how all this _actually works_ and flesh out my undertanding of building something like this in the best way possible. The ground up.

## The _how_

Leveraging my learnings on building parsers, interpreters and several other novel approaches this tool's goals are as followed:
* Perf, talking sub millisecond
* Portability, it needs to be a single binary
* Stability, goes hand in hand with simplicity
* Multi-dialect with passthrough for cloud warehouse native optimizations
* Avoid in-lining if possible

