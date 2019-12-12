#!/bin/bash
RUNHASKELL=${RUNHASKELL:-runhaskell}
EXECUTABLE=${EXECUTABLE:-./.stack-work/dist/e626a42b/build/MinHS
}
if test "$1" == "--no-color"; then
  $RUNHASKELL -cpp -DNOCOLOR -i./tests/driver ./tests/driver/Check.hs "$EXECUTABLE" "$2" "$3" "$4" "$5" "$6"
else
  $RUNHASKELL -cpp -i./tests/driver ./tests/driver/Check.hs "stack exec -- MinHS" "$1" "$2" "$3" "$4" "$5" "$6"
fi
