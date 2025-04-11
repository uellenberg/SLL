use ariadne::{Cache, Source};
use parking_lot::Mutex;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::{fs, io};

/// Keeps track of files in memory.
/// Note that file data is leaked,
/// so this should only be used for a
/// static cache.
#[derive(Debug, Default, Clone)]
pub struct FileCache {
    files: Arc<Mutex<HashMap<PathBuf, &'static str>>>,
    sources: Arc<Mutex<HashMap<PathBuf, &'static Source<&'static str>>>>,
}

impl FileCache {
    /// Gets the specified file, retrieving
    /// it from the disk if it doesn't exist.
    pub fn get(&self, file: &Path) -> io::Result<&'static str> {
        let mut files = self.files.lock();
        let mut sources = self.sources.lock();

        if let Some(file) = files.get(file) {
            return Ok(*file);
        }

        let data: &'static str = fs::read_to_string(file)?.leak();
        let source: &'static Source<&'static str> = Box::leak(Box::new(Source::from(data)));

        files.insert(file.to_path_buf(), data);
        sources.insert(file.to_path_buf(), source);

        Ok(data)
    }

    /// Gets the specified file, retrieving
    /// it from the disk if it doesn't exist.
    pub fn get_source(&self, file: &Path) -> io::Result<&'static Source<&'static str>> {
        let mut files = self.files.lock();
        let mut sources = self.sources.lock();

        if let Some(file) = sources.get(file) {
            return Ok(*file);
        }

        let data: &'static str = fs::read_to_string(file)?.leak();
        let source: &'static Source<&'static str> = Box::leak(Box::new(Source::from(data)));

        files.insert(file.to_path_buf(), data);
        sources.insert(file.to_path_buf(), source);

        Ok(source)
    }
}

impl Cache<Path> for FileCache {
    type Storage = &'static str;

    fn fetch(&mut self, path: &Path) -> Result<&Source<&'static str>, impl Debug> {
        self.get_source(path)
    }

    fn display<'a>(&self, path: &'a Path) -> Option<impl Display + 'a> {
        Some(Box::new(path.display()))
    }
}
