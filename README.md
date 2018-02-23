## What is this?

This is a tool that allows you to automatically upload
new APKs for existing games to Amazon app store.

Amazon is great, but they do not have an API to do this
programatically so we use Selenium and Chrome to use the
web interface for the uploading.

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
