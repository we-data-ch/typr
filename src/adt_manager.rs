use crate::Adt;

pub struct AdtManager {
    pub body: Adt,
    pub header: Adt
}

impl AdtManager {
    pub fn new() -> AdtManager {
        AdtManager {
            body: Adt(vec![]),
            header: Adt(vec![])
        }
    }

    pub fn add_to_body(self, adt: Adt) -> AdtManager {
        AdtManager {
            body: self.body.add(adt),
            ..self
        }
    }

    pub fn add_to_header(self, adt: Adt) -> AdtManager {
        AdtManager {
            header: self.header.add(adt),
            ..self
        }
    }

    pub fn get_adt_with_header(&self) -> Adt {
        self.header.clone().add(self.body.clone())
    }

    pub fn get_adt_without_header(&self) -> Adt {
        self.body.clone()
    }

    pub fn set_body(self, adt: Adt) -> AdtManager {
        AdtManager {
            body: adt,
            ..self
        }
    }

}
