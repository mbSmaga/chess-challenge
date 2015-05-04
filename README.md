# chess-challenge

This is my implementation of the Chess Challenge. The task is to find all unique configurations of a set of normal chess pieces on a chess board with dimensions M×N where none of the pieces is in a position to take any of the others.

### Installation and usage

Get the source code from this repository:
```
git clone https://github.com/mbSmaga/chess-challenge.git
```
Use default SBT_OPTS
```
SBT_OPTS="-Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256M"
```
Run the code using sbt:
```
sbt run
```

