## What is this?

This is a tool that allows you to automatically upload new version to amazon app store.

### How to use it?

You need [Scala SBT](https://www.scala-sbt.org/) and Java Development Kit 8 to build this tool.

Clone the repository, then run:

```
$ sbt stage
```

It will build the release to `target/universal/stage`.

Launch it with:

```
$ cd target/universal/stage
$ bin/amazon-appstore-automator
```

And follow the on-screen instructions.

An example app.conf can be found in root of this repository.
