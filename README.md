# kreins

run on 学科PC

```
$ cd 【project root】

$ sudo apt-get install openjdk-8-jre^C
$ java -version
openjdk version "1.8.0_222"

# if 1.8.x以外のバージョンが出力されたら
  $ sudo update-alternatives --config java
  # java-8-openjdk の番号を入力
  $ java -version
  openjdk version "1.8.0_222"
# end

$ sudo apt install openjdk-8-jdk
$ javac -version
javac 1.8.0_222

# if 1.8.x以外のバージョンが出力されたら
  $ sudo update-alternatives --config javac
  $ javac -version
  javac 1.8.0_222
# end

$ echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
$ sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
$ sudo apt-get update
$ sudo apt install sbt
$ sbt version
Getting org.scala-sbt sbt 1.2.8  (this may take some time)...
......
[info] 0.4.1

$ sbt assembly
......

$ cd target/scala-2.12
$ java -jar kreins-assembly-0.4.1.jar run -H HOST -p PORT -n NAME --dys ../../data/logbook.dys.gz
```
