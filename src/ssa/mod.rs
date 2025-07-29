pub mod block_builder;
pub mod cfg;
pub mod dominator;
pub mod ssa;
pub mod ssa_state;
pub mod liveness;

mod cfg_test;
mod dominator_test;
mod phi_insertion_test;
mod var_rename_test;