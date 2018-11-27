# `nanovg-port-rs`

A handmade port of the C vector graphics library [NanoVG](https://github.com/memononen/nanovg) in Rust.

My long-term goal is to have a NanoVG-backed GUI running in the browser via WebAssembly and WebGL, so using Rust's FFI seemed out of the question here.

From my minimal experience NanoVG is a really awesome project, but its code is esoteric and scarcely documented. I'm not very well-versed in OpenGL, 2D vector graphics, or the relevant math, so this porting process is quite arduous and I'm kind of flying by the seat of my pants. Ideally this code will evolve into a more idiomatic Rust codebase, but for now it's just a means to an end.
