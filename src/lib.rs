extern crate beam_file;
extern crate eetf;

mod module;
pub mod result;
pub mod error;

pub use module::Module;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {}
}
