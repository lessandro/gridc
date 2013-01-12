#!/bin/sh

gridc examples/ex$1.gridc > examples/ex$1.gridlang && gridlang.py examples/ex$1.gridlang

