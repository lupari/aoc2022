#! /bin/bash
touch src/main/resources/day$1.txt
cp src/main/scala/assignments/Day01.scala src/main/scala/assignments/Day$1.scala
perl -pi -e s,Day01,Day$1,g src/main/scala/assignments/Day$1.scala
perl -pi -e s,day01.txt,day$1.txt,g src/main/scala/assignments/Day$1.scala
cp src/test/scala/Test01.scala src/test/scala/Test$1.scala
perl -pi -e s,Day01,Day$1,g src/test/scala/Test$1.scala
perl -pi -e s,Test01,Test$1,g src/test/scala/Test$1.scala

