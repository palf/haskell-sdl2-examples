# Haskell SDL2 Examples

A port of LazyFoo's excellent SDL tutorials into Haskell, updated for SDL2

Check out the originals [here](http://lazyfoo.net/tutorials/SDL/index.php)

## Setup

The following paths can be renamed to whatever you like; I don't get the packages from Hackage as the SDL2 Image bindings provided below are simpler

These instructions will only work if you have SDL2 and SDL2Image on your system; as this is platform-specific I'm not going to cover it - Google is your friend!

    git clone https://github.com/palf/haskellSDL2Examples.git lazyfoo
    git clone https://github.com/polarina/sdl2.git hlibs/sdl2
    git clone https://github.com/ods94065/sdl2-image.git hlibs/sdl2-image

    cd lazyfoo
    cabal sandbox init --sandbox=sandbox
    cabal install ../hlibs/sdl2 ../hlibs/sdl2-image

## Running Examples

All examples can be executed through cabal

    cabal run lessonxx
    (where xx is the lesson number as two digits)

To run the first lesson,

    cabal run lesson01

## Lesson Output

* Lesson 01 - a white box which remains for two seconds
* Lesson 02 - "Hello World" text taken from a bitmap (remains for 2 seconds)
* Lesson 03 - a window with instructions to close it (remains until closed)
* Lesson 04 - instructions to press arrow keys, followed by content changing on key press
* Lesson 05 - a stretched image in the window
* Lesson 06 - a 'png loaded' message, loaded from a png
* Lesson 07 - a 'rendering texture' message
* Lesson 08 - a red box with a green outline, a blue strikethrough and yellow vertical dots
