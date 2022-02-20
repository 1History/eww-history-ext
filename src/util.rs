use std::time::{SystemTime, UNIX_EPOCH};

pub fn get_epoch_ms() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as i64
}
