use crate::Adt;

#[derive(Debug)]
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
        let header = adt.iter()
            .filter(|&x| x.is_undefined())
            .cloned().collect::<Vec<_>>();
        let body = adt.iter()
            .filter(|&x| !x.is_undefined())
            .cloned().collect::<Vec<_>>();
        AdtManager {
            body: self.body.add(Adt(body)),
            header: self.header.add(Adt(header))
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
