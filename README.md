# CCS-eval
CCS evaluator

This directory contain files for a CCS evaluator written in Mosml.


# How to install the evaluator

Step 1: Prerequisites:
- Mosml 
This program is programmed in Mosml and mosml must be installed before you can install MainEval. 
Go to https://mosml.org/ for instructinos on how to install mosml for Linux, MacOs, and Windows.

Step 2:
unzip the zip file "CSS Evaluator.zip"

Step 3:
go into the directory "CSS Evaluator"
'''
cd "CSS Evaluator"
'''

Step 4: 
If you have carried out step 1 - 3, run make to compile the evaluator
'''
make eval
'''

# How to evaluate and save complexity counters from the execution:
The command we use for running the evaluator is ./MainEval and it takes three inputs:

./MainEval trs-file-name output-file-name start-term

- a file name for the file with the CCS system you are evaluting, e.g., CCS/add.trs
- a file name for the file where you want to save your complexity data, e.g., OUT/test.out
- a start term that you want to evaluate, e.g., "add(s(s(0)),s(s(0)))"

# Example: Two test files are provided in the CCS-directory.
In the following example we evaluate an add-term "add(s(s(0)),s(0))" using the defined 
function add specified in CCS/add.trs. We want to the information about the number of 
required calls and rewritings required for the execution to be stored in a file 
"OUT/test.out".

'''
./MainEval "CCS/add.trs" OUT/test.out "add(s(s(0)),s(0))"
'''

This will prompt the rewriting trace in the terminal and save the additional information
in the test.out in the OUT-directory.