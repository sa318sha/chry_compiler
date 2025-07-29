// // Log level constants
// pub const LOG_LEVEL_ERROR: u8 = 1;
// pub const LOG_LEVEL_WARN: u8 = 2;
// pub const LOG_LEVEL_INFO: u8 = 3;
// pub const LOG_LEVEL_DEBUG: u8 = 4;

// // Set the compile-time log level here
// pub const COMPILE_TIME_LOG_LEVEL: u8 = LOG_LEVEL_DEBUG;

// // Macros
// #[macro_export]
// macro_rules! log_debug {
//     ($($arg:tt)*) => {
//         {
//             const ENABLED: bool = crate::logging::COMPILE_TIME_LOG_LEVEL >= crate::logging::LOG_LEVEL_DEBUG;
//             if ENABLED {
//                 println!("[DEBUG] {}", format_args!($($arg)*));
//             }
//         }
//     };
// }

// #[macro_export]
// macro_rules! log_warn {
//     ($($arg:tt)*) => {
//         {
//             const ENABLED: bool = crate::logging::COMPILE_TIME_LOG_LEVEL >= crate::logging::LOG_LEVEL_WARN;
//             if ENABLED {
//                 println!("[WARN] {}", format_args!($($arg)*));
//             }
//         }
//     };
// }

// // #[macro_export]
// // macro_rules! log_error {
// //     ($($arg:tt)*) => {
// //         {
// //             const ENABLED: bool = logging::COMPILE_TIME_LOG_LEVEL >= logging::LOG_LEVEL_ERROR;
// //             if ENABLED {
// //                 println!("[ERROR] {}", format_args!($($arg)*));
// //             }
// //         }
// //     };
// // }
