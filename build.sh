#!/bin/sh
PROJECTNAME="Turbocharged"

BuildError () {
    PROJECTNAME=
    echo Build failed, aborting...
    exit 1
}

echo "Converting modules..."
# Build ROM
python3 Tools/xmconv.py Audio/Modules/Turbo.xm Audio/Modules/Turbo.bin -t 4 -m 176

echo Assembling...
rgbasm -o $PROJECTNAME.obj -p 255 Main.asm
if test $? -eq 1; then
    BuildError
fi

echo Linking...
rgblink -p 255 -o $PROJECTNAME.gb -n $PROJECTNAME.sym $PROJECTNAME.obj
if test $? -eq 1; then
    BuildError
fi

echo Fixing...
rgbfix -v -p 255 $PROJECTNAME.gb

echo Cleaning up...
rm $PROJECTNAME.obj

echo Build complete.

# unset vars
PROJECTNAME=
echo "** Build finished with no errors **"
