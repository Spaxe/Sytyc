echo "Moving into src ..."
cd src
echo "Compiling Sytyc ..."
ghc --make -O3 -H14m index.hs
ghc --make -O3 -H14m problem.hs
ghc --make -O3 -H14m feedback.hs
echo "Copying executables ..."
cd ..
mv src/index.exe index.cgi
mv src/problem.exe problem.cgi
mv src/feedback.exe feedback.cgi
echo "Cleaning up ..."
cd src
rm *.o *.hi
cd ..