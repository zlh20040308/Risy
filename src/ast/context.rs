pub struct IrGenContext {
    counter: usize,
}

impl IrGenContext {
    pub fn new() -> Self {
        IrGenContext { counter: 0 }
    }

    pub fn next_temp(&mut self) -> String {
        let name = format!("%{}", self.counter);
        self.counter += 1;
        name
    }
}
