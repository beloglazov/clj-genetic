#!/bin/sh
lein clean, jar, pom
scp pom.xml clj-genetic-$1.jar clojars@clojars.org:
rm clg-genetic-$1.jar
