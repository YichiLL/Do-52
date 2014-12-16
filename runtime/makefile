JFLAGS = -g
JVMFLAGS = -cp
JC = javac
JVM = java
.SUFFIXES: .java .class
.java.class:
	$(JC) $(JFLAGS) $*.java

CLASSES = \
        Card.java \
	Set.java\
        Deck.java \
	Utility.java\
	Player.java\
	MyPlayer.java\
	Game.java \
        main.java 

MAIN= main

default = classes

classes : $(CLASSES:.java=.class)

run: classes 
	$(JVM) $(JVMFLAGS) . $(MAIN)
clean:
	 $(RM) *.class