echo "Moving into src ..."
cd src
echo "Compiling Sytyc ..."
ghc --make -O2 -H14m index.hs
ghc --make -O2 -H14m problem.hs
ghc --make -O2 -H14m problem_list.hs
ghc --make -O2 -H14m faq.hs
ghc --make -O2 -H14m feedback.hs
echo "Copying executables ..."
cd ..
mv src/index.exe index.cgi
mv src/problem.exe problem.cgi
mv src/problem_list.exe problem_list.cgi
mv src/feedback.exe feedback.cgi
mv src/faq.exe faq.cgi
echo "Cleaning up ..."
cd src
# rm *.o *.hi
cd ..
