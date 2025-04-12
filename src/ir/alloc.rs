use num_traits::PrimInt;
use std::borrow::Cow;
use std::collections::HashMap;
use std::iter;

/// Data representing a type
/// at the assembly level.
pub struct TypeData {
    /// The type's size (in bytes).
    pub size: u32,

    /// The type's alignment (in bytes).
    pub align: u32,
}

/// Used to determine the offset
/// to allocate variables on
/// the stack.
///
/// This is intended to run in two passes.
/// During the first pass, it will determine
/// necessary information, which can be
/// used in the second pass.
pub struct StackAllocator<'a> {
    /// The number of bytes already occupied in the
    /// stack, relative to the stack pointer.
    /// This will be excluded from stack_size,
    /// as it's already allocated.
    offset: u32,

    /// The required alignment for the
    /// stack pointer before and after
    /// this function call.
    alignment: u32,

    /// A list of bytes (true = occupied).
    /// The size of this list represents
    /// the maximum number of bytes
    /// required by the stack.
    blocks: Vec<bool>,

    /// A map of live variable names
    /// to their stack position and
    /// size.
    variables: HashMap<Cow<'a, str>, (u32, u32)>,
}

impl<'a> StackAllocator<'a> {
    /// Creates a new StackAllocator, with
    /// the stack pointer at the specified
    /// alignment and a certain number of
    /// bytes (offset) already allocated.
    pub fn new(align: u32, offset: u32) -> StackAllocator<'a> {
        StackAllocator {
            offset,
            alignment: align,
            blocks: vec![true; offset as usize],
            variables: HashMap::new(),
        }
    }

    /// Allocates a new variable, returning its
    /// stack offset.
    pub fn create(&mut self, name: Cow<'a, str>, size: u32, align: u32) -> u32 {
        assert!(align.is_power_of_two());

        // There's no way for us to statically get a higher
        // alignment than what's given by the stack pointer.
        assert!(align <= self.alignment);

        // Search for an existing space.
        'search: for start in (0..self.blocks.len()).step_by(align as usize) {
            // Optionally resize the vec if it doesn't
            // have enough space for us to exist at this
            // position.
            // Max index is start + size - 1,
            // and max index for the vec is
            // blocks.len() - 1, so we can drop
            // the -1 in the comparison.
            if self.blocks.len() < start + size as usize {
                let needed = (start + size as usize) - self.blocks.len();
                self.blocks.extend(iter::repeat_n(false, needed));
            }

            for offset in (0..size) {
                if self.blocks[start + offset as usize] {
                    // Occupied.
                    continue 'search;
                }
            }

            // We found a valid spot.
            self.write_var(start as u32, name, size);
            return start as u32;
        }

        // No space was found.
        // We need to add it.
        let extra_space =
            (align_to(self.blocks.len(), align as usize) - self.blocks.len()) + size as usize;
        self.blocks.extend(iter::repeat_n(false, extra_space));

        // blocks.len() represents the index past the
        // last one, so subtracting size gives us size
        // valid items (e.g., len() - 2 gives 2 valid items).
        let pos = self.blocks.len() as u32 - size;

        self.write_var(pos, name, size);

        pos
    }

    /// Deallocates the given variable.
    pub fn drop(&mut self, name: &Cow<'a, str>) {
        let var = self
            .variables
            .remove(name)
            .expect("Tried to drop non-existent variable!");

        // start..(start + size)
        for i in (var.0)..(var.0 + var.1) {
            self.blocks[i as usize] = false;
        }
    }

    /// Determines how large the stack is, respecting
    /// alignment.
    pub fn stack_size(&self) -> u32 {
        align_to(self.blocks.len() as u32 - self.offset, self.alignment)
    }

    /// Gets the stack position of a certain variable.
    pub fn get(&self, name: &Cow<'a, str>) -> u32 {
        self.variables[name].0
    }

    /// Creates a variable at the specified position.
    fn write_var(&mut self, pos: u32, name: Cow<'a, str>, size: u32) {
        for i in pos..(pos + size) {
            self.blocks[i as usize] = true;
        }

        self.variables.insert(name, (pos, size));
    }
}

/// Rounds up to make num aligned to align.
fn align_to<T: PrimInt>(num: T, align: T) -> T {
    ((num + align - T::one()) / align) * align
}
