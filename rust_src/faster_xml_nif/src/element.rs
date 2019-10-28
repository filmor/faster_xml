use rustler::Encoder;


pub(crate) struct Element<Enc: Encoder> {
    enc: Enc
}

impl<Enc: Encoder> Element<Enc> {
    fn new() -> Self {
        unimplemented!()
    }
}