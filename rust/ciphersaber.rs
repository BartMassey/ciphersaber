// Copyright © 2012 Bart Massey
// [This program is licensed under the "MIT License"]
// Please see the file COPYING in the source
// distribution of this software for license terms.

// CipherSaber
// http://ciphersaber.gurus.com

extern crate getopts;
extern crate rpassword;

use getopts::Options;
use rpassword::read_password;
use std::default::Default;
use std::env;
use std::fs::File;
use std::io::{stdin, stdout, Read, Write};
use std::path::Path;
use std::error::Error;

fn mkiv(ivb: &mut [u8;10]) {
    // http://rustbyexample.com/std_misc/file/open.html
    let urandom = Path::new("/dev/urandom");
    let mut fd = match File::open(&urandom) {
        Err(why) => panic!("couldn't open {}: {}",
                           urandom.display(),
                           why.description()),
        Ok(file) => file
    };
    let ok = fd.read_exact(ivb);
    match ok {
        Ok(()) => (),
        _ => panic!("couldn't read {}", urandom.display())
    }
}

enum Dirn {
    Encrypt,
    Decrypt
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut opts = Options::new();
    opts.optflag("e", "encrypt", "encrypt input to output");
    opts.optflag("d", "decrypt", "decrypt input to output");
    opts.optopt("k", "key", "KEY", "key on command line (dangerous)");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => panic!(f.to_string())
    };
    let en = matches.opt_present("e");
    let de = matches.opt_present("d");
    if en && de {
        panic!("cannot both encrypt and decrypt");
    }
    if !en && !de {
        panic!("must encrypt or decrypt");
    }
    let dirn = if en {
        Dirn::Encrypt
    } else {
        assert!(de);
        Dirn::Decrypt
    };
    let mut keybytes: Vec<u8> = vec![];
    match matches.opt_str("k") {
        Some(s) => keybytes.extend_from_slice(s.as_bytes()),
        _ => {
            let ok = read_password();
            match ok {
                Ok(s) => keybytes.extend_from_slice(s.as_bytes()),
                _ => panic!("password read error")
            }
        }
    };
    
    let mut sin = stdin();
    let mut sout = stdout();
    let mut ivb :[u8;10] = [Default::default();10];
    match dirn {
        Dirn::Encrypt => {
            mkiv(&mut ivb);
            sout.write(&mut ivb).expect("write failed");
        }
        Dirn::Decrypt => {
            let ok = sin.read_exact(&mut ivb);
            match ok {
                Ok(()) => (),
                _ => panic!("couldn't read iv")
            }
        }
    };
    keybytes.extend_from_slice(&ivb);

    let mut state: [u8;256] = [Default::default();256];
    for k in 0..256 {
        state[k] = k as u8;
    }
    let kl = keybytes.len();
    let mut j = 0;
    for k in 0..256 {
        j = (j + state[k] as usize + keybytes[k % kl] as usize) & 0xff;
        let tmp = state[k];
        state[k] = state[j];
        state[j] = tmp;
    }

    let mut i: usize = 0;
    let mut j: usize = 0;
    for ch in sin.bytes() {
        i = (i + 1) & 0xff;
        j = (j + state[i] as usize) & 0xff;
        let si = state[i];
        let sj = state[j];
        state[i] = sj;
        state[j] = si;
        let plain = ch.unwrap();
        let cipher = state[(si as usize + sj as usize) & 0xff] ^ plain;
        let b = [cipher];
        match sout.write(&b) {
            Ok(_) => (),
            _ => panic!("write error")
        };
    }
}
