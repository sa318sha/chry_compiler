#!/bin/bash
RUSTFLAGS="-A dead_code" cargo build "$@"