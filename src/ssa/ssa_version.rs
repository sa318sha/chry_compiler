use std::collections::HashMap;

use crate::types::{hir_types::TempId, ssa_types::SSATempId};

pub struct SSAVersion {
    anon_temp_counter: usize,
    hir_to_ssa_temp: HashMap<TempId, SSATempId>,
    var_version: HashMap<String, usize>,
}

impl SSAVersion {
    pub fn new() -> SSAVersion {
        return SSAVersion {
            anon_temp_counter: 0,
            hir_to_ssa_temp: HashMap::new(),
            var_version: HashMap::new(),
        };
    }

    pub fn convert_hir_temp_to_ssa_temp(
        &mut self,
        hir_temp: &TempId,
        from_variable: Option<String>,
    ) -> SSATempId {
        if let Some(var_name) = from_variable {
            let version = self.next_version(&var_name);
            // let version = self.var_version.entry(var_name.clone()).or_insert(0);
            let temp = SSATempId {
                name: var_name,
                version: version,
            };
            // *version += 1;
            return temp;
        } else {
            if let Some(temp) = self.hir_to_ssa_temp.get(hir_temp) {
                return temp.clone();
            }
            let temp = SSATempId {
                name: "_tmp".to_string(),
                version: self.anon_temp_counter,
            };
            self.anon_temp_counter += 1;
            self.hir_to_ssa_temp.insert(*hir_temp, temp.clone());
            return temp;
        }
    }

    pub fn next_version(&mut self, var: &String) -> usize {
        let val = self.var_version.entry(var.to_string()).or_insert(0);
        *val += 1;
        *val
    }
}
