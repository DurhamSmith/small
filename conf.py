#!/usr/bin/env python3

from os.path import join, dirname, realpath, expandvars

# Extensions: add 'sphinxcontrib.cldomain' and 'sphinxcontrib.hyperspec',
# just like this example:
extensions = [
    'sphinx.ext.intersphinx',
    'sphinxcontrib.cldomain',
    'sphinxcontrib.hyperspec'
]

# --- CL domain customizations:
#
# cl_systems: The systems and packages from which to extract documentation:
#
# name - The name of the system to load.
# path - The path to the system.
# packages - A list of the packages to extract symbol information from.
#
# Note: This conf.py sits in a subdirectory below ("../"), relative to where
# the "my-system.asd" system description file lives:
cl_systems = [{"name": "small",
               "path": "/home/dd/quicklisp/local-projects/small",
               "packages": ["chem-obj.lisp", "dna.lisp"]}]

print(cl_systems)

# cl_quicklisp: The default is $HOME/quicklisp. Shown here for completeness,
# and you can comment it out:
cl_quicklisp = expandvars('$HOME/quicklisp')

# Ensure that the default highlighting language is CL:
highlight_language = 'common-lisp'

# For developer debugging only (and the curious, although, it did kill the cat!)
# Currently ``True`` or ``False`` to output the JSON collected from cldomain.
cl_debug = True
