# Implementing a Monte Carlo Simulation in Haskell User Guide
Hello this is a guide on how to successfully run our project's program please read the entire document, skipping sections only when you already know that they apply to you. This installation process was verified on a blank Windows 10 machine that had nothing related installed on it. 

# Install Haskell (with Stack and Cabal)
Install the Haskell programming language via ghccup with this link: https://www.haskell.org/ghcup/ follow whichever install guide is appropriate for your system, the importpant part is to be sure you install the following: Haskell, stack, cabal.

# Install Git
Our project is hosted on GitHub, so it uses Git as the version control system, you need to make sure you have this installed as well download is available at the following link: https://git-scm.com/downloads

# Clone The Repository
If you are already reading this on the GitHub repository main page then click the green code button towards the top and copy the http link provided, open a terminal and navigate to where you'd like the project to be cloned to and the run the following command: <code>git clone x</code> with <code>x</code> being the link you copied.

If you are reading this from the README present in the file structure of this project from an alternative source, visit the GitHub at the following link and follow the above instruction: https://github.com/griffinnewbold/COMS-4995-Project

# Navigate into the monteParSim directory
Once you have cloned the repository run the following command: <code>cd COMS-4995-Project\monteParSim</code>

# Build using <code>stack build</code>
If it is the very first time doing this then it will take a bit of time, you shouldn't have any issues but in the event you do visit the <code>stack.yaml</code> file and scroll down to <code>extra-deps</code> and install the following packages manually with the <code>stack install package</code> command the package does not include any numbers just characters belonging to the english alphabet.

You can execute the main method of the program and specify relative alternative options with the following: <code>stack build --exec "monteParSim-exe +RTS -N4"</code> this command enables runtime system options and runs with 4 cores at the program's disposal, this is only relevant for the parallel portions of the code.  

# Verify Functionality using <code>stack test</code>
To ensure nothing broke prior to executing the code on your own, you can run this command to verify via our unit tests that the code we wrote 
1. Does not error out 
2. Provides accurate values 

Just like with the build command you can specify additional options with the following: <code>stack test --test-arguments "+RTS -N4"</code> this command enables runtime system options and runs with 4 cores at the program's disposal, this is only relevant for the parallel portions of the code.  
