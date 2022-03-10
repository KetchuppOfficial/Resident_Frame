# Resident frame

## General information

This program is written in assembler language for MS-DOS (assembler: TASM). It's a residential program that hooks interrupts **int 08h** (real-time timer) and **int 09h** (keyboard) and prints a frame with values of 8 registers: **AX**, **BX**, **CX**, **DX**, **SI**, **DI**, **BP**, **SP**.

## Usage

After running this program you can do whatever you want in DOS as the program will work on the background. Press **I** to make the frame appear, **J** - to disappear.

## Unloading after using

Don't forget to unload this program after using. It can be done by pressing *Alt + F5*. You should choose this program in appeared list (makred by a point on the left) and press *Reload*.

## Features

As far as double buffering is used pressing **J** leads to appearing correct piece of image "under" the disappeared frame.

## Example of the frame

![Frame](https://github.com/KetchuppOfficial/Resident_Frame/blob/main/Frame_Example.jpg)
