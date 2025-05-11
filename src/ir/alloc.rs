use num_traits::PrimInt;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::mem::ManuallyDrop;
use std::{iter, ptr};

/// Data representing a type
/// at the assembly level.
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct TypeData {
    /// The type's size (in bytes).
    pub size: u32,

    /// The type's alignment (in bytes).
    pub align: u32,
}

/// Used to allocate variables and
/// temporaries.
pub trait UnifiedAllocator<'a> {
    type Ctx;

    type Read1;
    type Read4;
    type Read8;

    type Read1Direct;
    type Read4Direct;
    type Read8Direct;

    /// Allocates a variable, either as a register
    /// or on the stack.
    fn alloc_variable(&mut self, name: Cow<'a, str>, ty: TypeData);

    /// Deallocates a variable.
    fn drop_variable(&mut self, name: &Cow<'a, str>);

    /// Allocates a temporary register, erroring
    /// on failure.
    ///
    /// This register must be dropped using drop_temporary.
    fn alloc_temporary(&mut self, size: u32) -> TemporaryRegister;

    /// Drops a temporary register
    /// allocated using alloc_temporary.
    fn drop_temporary(&mut self, temporary: TemporaryRegister);

    /// Drops a RegMaybeTemporary if it contains
    /// temporaries, otherwise does nothing.
    fn drop_maybe_temporary<const Size: usize>(
        &mut self,
        maybe_temporary: RegMaybeTemporary<Size>,
    ) {
        match maybe_temporary {
            // Nothing to drop.
            RegMaybeTemporary::Register(_) => {}
            RegMaybeTemporary::Temporary(temps) => {
                for temp in temps {
                    self.drop_temporary(temp);
                }
            }
        }
    }

    /// Gets information about a variable.
    /// To read/write data from/to it, use the
    /// read and write methods.
    fn get(&self, name: &Cow<'a, str>) -> TypeData;

    /// Reads 1 byte into a register.
    /// This is not intended for arbitrary reads.
    /// Rather, it should be used to read an entire
    /// data type, or an entire data type
    /// stored within another data type.
    ///
    /// The result returned by this must
    /// be given to drop_maybe_temporary.
    fn read_1(&mut self, ctx: &mut Self::Ctx, name: &Cow<'a, str>, offset: u32) -> Self::Read1;

    /// Reads 4 bytes into a register.
    /// This is not intended for arbitrary reads.
    /// Rather, it should be used to read an entire
    /// data type, or an entire data type
    /// stored within another data type.
    ///
    /// The result returned by this must
    /// be given to drop_maybe_temporary.
    fn read_4(&mut self, ctx: &mut Self::Ctx, name: &Cow<'a, str>, offset: u32) -> Self::Read4;

    /// Reads 8 bytes into a register.
    /// This is not intended for arbitrary reads.
    /// Rather, it should be used to read an entire
    /// data type, or an entire data type
    /// stored within another data type.
    ///
    /// The result returned by this must
    /// be given to drop_maybe_temporary.
    fn read_8(&mut self, ctx: &mut Self::Ctx, name: &Cow<'a, str>, offset: u32) -> Self::Read8;

    /// This version of read_1 only returns
    /// if it can be achieved without any
    /// extra assembly.
    ///
    /// It returns a register for a variable only
    /// if that variable is already in the register
    /// and can be accessed directly.
    fn read_1_direct(&self, name: &Cow<'a, str>, offset: u32) -> Option<Self::Read1Direct>;

    /// This version of read_4 only returns
    /// if it can be achieved without any
    /// extra assembly.
    ///
    /// It returns a register for a variable only
    /// if that variable is already in the register
    /// and can be accessed directly.
    fn read_4_direct(&self, name: &Cow<'a, str>, offset: u32) -> Option<Self::Read4Direct>;

    /// This version of read_8 only returns
    /// if it can be achieved without any
    /// extra assembly.
    ///
    /// It returns a register for a variable only
    /// if that variable is already in the register
    /// and can be accessed directly.
    fn read_8_direct(&self, name: &Cow<'a, str>, offset: u32) -> Option<Self::Read8Direct>;

    /// Writes 1 byte from the given register into
    /// the variable at the specified offset.
    /// The offset is not used when reading from
    /// the register.
    ///
    /// This is not intended for arbitrary writes.
    /// Rather, it should be used to write an entire
    /// data type, or an entire data type
    /// stored within another data type.
    fn write_1(&mut self, ctx: &mut Self::Ctx, reg: &Self::Read1, name: &Cow<'a, str>, offset: u32);

    /// Writes 4 bytes from the given register into
    /// the variable at the specified offset.
    /// The offset is not used when reading from
    /// the register.
    ///
    /// This is not intended for arbitrary writes.
    /// Rather, it should be used to write an entire
    /// data type, or an entire data type
    /// stored within another data type.
    fn write_4(&mut self, ctx: &mut Self::Ctx, reg: &Self::Read1, name: &Cow<'a, str>, offset: u32);

    /// Writes 8 bytes from the given register into
    /// the variable at the specified offset.
    /// The offset is not used when reading from
    /// the register.
    ///
    /// This is not intended for arbitrary writes.
    /// Rather, it should be used to write an entire
    /// data type, or an entire data type
    /// stored within another data type.
    fn write_8(&mut self, ctx: &mut Self::Ctx, reg: &Self::Read1, name: &Cow<'a, str>, offset: u32);

    /// Determines how large the stack is, respecting
    /// alignment.
    fn stack_size(&self) -> u32;

    /// Returns a list of the registers
    /// that are used by the program.
    fn used_regs(&self) -> impl Iterator<Item = &'static str>;
}

/// Used to allow a read to load temporaries,
/// if necessary, or to use existing registers.
/// Size is the number of registers.
#[derive(Debug, Eq, PartialEq)]
pub enum RegMaybeTemporary<const Size: usize> {
    Temporary([TemporaryRegister; Size]),
    Register([&'static str; Size]),
}

impl<const Size: usize> RegMaybeTemporary<Size> {
    /// Gets the names of the registers
    /// stored inside this container.
    pub fn names(&self) -> [&'static str; Size] {
        let mut output = [""; Size];

        match self {
            RegMaybeTemporary::Temporary(temps) => {
                for i in 0..Size {
                    output[i] = temps[i].name;
                }
            }
            RegMaybeTemporary::Register(registers) => {
                for i in 0..Size {
                    output[i] = registers[i];
                }
            }
        }

        output
    }

    pub fn try_conv<const OtherSize: usize>(self) -> Option<RegMaybeTemporary<OtherSize>> {
        if Size == OtherSize {
            let to_move = ManuallyDrop::new(self);

            let read_ptr = &to_move as *const _ as *const RegMaybeTemporary<Size>;
            assert!(read_ptr.is_aligned());

            // SAFETY: We've verified that the types are the same size,
            //         and verified alignment.
            Some(unsafe { ptr::read(read_ptr as *const RegMaybeTemporary<OtherSize>) })
        } else {
            None
        }
    }
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
    /// stack, relative to the frame pointer.
    /// This will be excluded from stack_size,
    /// as it's already allocated.
    offset: u32,

    /// The number of bytes allocated
    /// at the stack pointer.
    /// If the stack pointer points to an existing chunk
    /// of 4 bytes, then this should be 4.
    /// If the stack pointer points to the next available space,
    /// then this should be 0.
    at_sp: u32,

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
    variables: HashMap<Cow<'a, str>, (i32, TypeData)>,
}

impl<'a> StackAllocator<'a> {
    /// Creates a new StackAllocator, with
    /// the stack pointer at the specified
    /// alignment and a certain number of
    /// bytes already allocated.
    ///
    /// at_sp specifies the number of bytes allocated
    /// at the stack pointer.
    /// If the stack pointer points to an existing chunk
    /// of 4 bytes, then this should be 4.
    /// If the stack pointer points to the next available space,
    /// then this should be 0.
    pub fn new(align: u32, at_sp: u32) -> StackAllocator<'a> {
        StackAllocator {
            offset: 0,
            at_sp,
            alignment: align,
            blocks: vec![true; at_sp as usize],
            variables: HashMap::new(),
        }
    }

    /// Allocates a new variable, returning its
    /// stack offset.
    pub fn create(&mut self, name: Cow<'a, str>, type_data: TypeData) -> i32 {
        assert!(type_data.align.is_power_of_two());

        // There's no way for us to statically get a higher
        // alignment than what's given by the stack pointer.
        assert!(type_data.align <= self.alignment);

        // Search for an existing space.
        'search: for mut start in (0..self.blocks.len()).step_by(type_data.align as usize) {
            // Bump alignment based on offset.
            // The real start of FP is before offset,
            // so we need to calculate start such that start + offset
            // is aligned.
            start = align_to(start + self.offset as usize, type_data.align as usize)
                - self.offset as usize;

            // Optionally resize the vec if it doesn't
            // have enough space for us to exist at this
            // position.
            // Max index is start + size - 1,
            // and max index for the vec is
            // blocks.len() - 1, so we can drop
            // the -1 in the comparison.
            if self.blocks.len() < start + type_data.size as usize {
                let needed = (start + type_data.size as usize) - self.blocks.len();
                self.blocks.extend(iter::repeat_n(false, needed));
            }

            for offset in (0..type_data.size) {
                if self.blocks[start + offset as usize] {
                    // Occupied.
                    continue 'search;
                }
            }

            // We found a valid spot.
            self.register_variable(start as i32, name, type_data);
            return start as i32;
        }

        // No space was found.
        // We need to add it.
        let extra_space = (align_to(self.blocks.len(), type_data.align as usize)
            - self.blocks.len())
            + type_data.size as usize;
        self.blocks.extend(iter::repeat_n(false, extra_space));

        // blocks.len() represents the index past the
        // last one, so subtracting size gives us size
        // valid items (e.g., len() - 2 gives 2 valid items).
        let pos = self.blocks.len() as u32 - type_data.size;

        self.register_variable(pos as i32, name, type_data);

        pos as i32
    }

    /// Deallocates the given variable,
    /// returning whether the variable existed.
    pub fn drop(&mut self, name: &Cow<'a, str>) -> bool {
        let Some(var) = self.variables.remove(name) else {
            return false;
        };

        // start..(start + size)
        for i in (var.0)..(var.0 + var.1.size as i32) {
            // Negative offsets aren't allocated,
            // but assigned, so we can't do anything
            // with them.
            if i < 0 {
                continue;
            }

            self.blocks[i as usize] = false;
        }

        true
    }

    /// Determines how large the stack is, respecting
    /// alignment.
    pub fn stack_size(&self) -> u32 {
        // at_sp is excluded from the calculations because
        // it has no effect on stack size.
        // The offset needs to be included such that offset + stack_size
        // has the correct alignment.
        // This computes the total required size, then removed offset
        // to get stack_size.
        align_to(
            self.blocks.len() as u32 - self.at_sp + self.offset,
            self.alignment,
        ) - self.offset
    }

    /// Determines the number of bytes padded
    /// to the end of the SP to give it 8-byte
    /// alignment.
    ///
    /// If all stack offsets are incremented
    /// by this number, it will push the padding
    /// to the front.
    pub fn post_padding(&self) -> u32 {
        self.stack_size() - (self.blocks.len() as u32 - self.at_sp)
    }

    /// Gets the stack position of a certain variable.
    pub fn get(&self, name: &Cow<'a, str>) -> Option<(i32, TypeData)> {
        self.variables.get(name).copied()
    }

    /// Creates a variable at the specified position.
    ///
    /// This is used to manually create variables that
    /// are tied to specific positions in the stack.
    ///
    /// It is invalid to override an existing variable!
    pub fn register_variable(&mut self, pos: i32, name: Cow<'a, str>, type_data: TypeData) {
        // Optionally resize the vec if it doesn't
        // have enough space for us to exist at this
        // position.
        // Max index is pos + size - 1,
        // and max index for the vec is
        // blocks.len() - 1, so we can drop
        // the -1 in the comparison.
        if (self.blocks.len() as i32) < pos + type_data.size as i32 {
            let needed = (pos + type_data.size as i32) - self.blocks.len() as i32;

            if needed < 0 {
                panic!("Negative needed blocks!");
            }
            self.blocks.extend(iter::repeat_n(false, needed as usize));
        }

        for i in pos..(pos + type_data.size as i32) {
            // Negative stack offsets are handled
            // manually.
            if pos < 0 {
                continue;
            }

            self.blocks[i as usize] = true;
        }

        if !self.variables.insert(name, (pos, type_data)).is_none() {
            panic!("write_var overrode a variable!");
        }
    }

    /// Updates the offset.
    /// This MUST NOT be used
    /// while any variables exist.
    pub fn set_offset(&mut self, offset: u32) {
        if !self.variables.is_empty() {
            panic!("Tried to set offset while variables exist!");
        }

        self.offset = offset;
    }
}

/// Used to allocate variables onto
/// registers.
pub struct RegisterAllocator<'a> {
    /// Register name -> size (in bytes).
    register_sizes: HashMap<&'static str, u32>,

    /// A list of available registers
    /// and their size (in bytes),
    /// ordered by size in descending
    /// order.
    available_regs: Vec<(&'static str, u32)>,

    /// A list of currently allocated variables
    /// and the allocations they contain.
    variables: HashMap<Cow<'a, str>, RegisterAllocation>,

    /// A list of the registers that were used
    /// by the register allocator.
    used_regs: HashSet<&'static str>,

    // TODO: This needs to be per-register-size.
    /// The minimum required registers for the allocator
    /// to function.
    /// If a normal allocation attempts to go below this
    /// amount, it will fail.
    /// However, temporary allocations are allowed to
    /// exceed this amount.
    min_available: u32,
}

impl<'a> RegisterAllocator<'a> {
    /// Creates a new RegisterAllocator.
    ///
    /// available_registers is the list of registers
    /// currently available. It's formatted as (name, bytes).
    ///
    /// min_available is the minimum number of registers
    /// that need to be maintained for temporary allocations.
    pub fn new(
        available_registers: impl IntoIterator<Item = (&'static str, u32)> + Clone,
        min_available: u32,
    ) -> Self {
        Self {
            register_sizes: available_registers.clone().into_iter().collect(),
            available_regs: available_registers.into_iter().collect(),
            variables: HashMap::new(),
            used_regs: HashSet::new(),
            min_available,
        }
    }

    /// Returns a list of the registers
    /// that are used by the program.
    pub fn used_regs(&self) -> impl Iterator<Item = &'static str> {
        self.used_regs.iter().copied()
    }

    /// Removes the specified registers from the pool
    /// of available registers.
    ///
    /// If the register is currently in use, this won't
    /// do anything to it.
    /// However, it will return a list of the names of
    /// all the variables that need to be dropped
    /// to take the remaining registers.
    pub fn take_registers(
        &mut self,
        registers: impl IntoIterator<Item = &'static str>,
    ) -> Vec<Cow<'a, str>> {
        let registers: HashSet<&'static str> = registers.into_iter().collect();
        let mut used_reg_vars = vec![];

        // Find variables using one of the given registers.
        for (name, alloc) in &self.variables {
            for (reg, _) in &alloc.regs {
                if registers.contains(reg) {
                    used_reg_vars.push(name.clone());
                    break;
                }
            }
        }

        // Remove registers from the available pool.
        self.available_regs
            .retain(|(reg, _)| !registers.contains(reg));

        // Track used registers.
        for reg in registers {
            self.used_regs.insert(reg);
        }

        self.sort_available_regs();
        used_reg_vars
    }

    /// Adds the given registers back
    /// to the pool of available registers.
    ///
    /// This MUST be used in combination with
    /// take_registers, and only when you can
    /// guarantee that the given register isn't
    /// being used by a variable.
    pub fn return_registers(&mut self, registers: impl IntoIterator<Item = &'static str>) {
        let registers: HashSet<&'static str> = registers.into_iter().collect();

        #[cfg(debug_assertions)]
        {
            // Ensure that no variable is using one of
            // these registers.
            for (name, alloc) in &self.variables {
                for (reg, _) in &alloc.regs {
                    if registers.contains(reg) {
                        panic!(
                            "Improper return_registers usage detected: register {reg} is in use by {name}!"
                        );
                    }
                }
            }
        }

        // Add back to the available pool.
        self.available_regs.extend(
            registers
                .into_iter()
                .map(|reg| (reg, self.register_sizes[reg])),
        );

        self.sort_available_regs();
    }

    /// Registers a new variable bound
    /// to the given registers.
    ///
    /// This MUST be used in combination with
    /// take_registers.
    /// Once registers are taken, they can be
    /// used here to manually create a variable.
    ///
    /// This should only be used to create
    /// variable bindings for registers that
    /// are already allocated, such as function
    /// arguments passed as registers.
    pub fn register_variable(
        &mut self,
        name: Cow<'a, str>,
        registers: impl IntoIterator<Item = &'static str>,
        ty: TypeData,
    ) {
        let registers: HashSet<&'static str> = registers.into_iter().collect();

        #[cfg(debug_assertions)]
        {
            // Ensure that no variable is using one of
            // these registers.
            for (name, alloc) in &self.variables {
                for (reg, _) in &alloc.regs {
                    if registers.contains(reg) {
                        panic!(
                            "Improper register_variable usage detected: register {reg} is in use by {name}!"
                        );
                    }
                }
            }
        }

        let regs = registers
            .into_iter()
            .map(|reg| (reg, self.register_sizes[reg]))
            .collect();

        if !self
            .variables
            .insert(
                name,
                RegisterAllocation {
                    regs,
                    ty,
                    freed: false,
                },
            )
            .is_none()
        {
            panic!("register_variable overrode a variable!");
        }

        self.sort_available_regs();
    }

    /// Tries to allocate a variable into one
    /// or more registers.
    pub fn try_alloc(&mut self, name: Cow<'a, str>, ty: TypeData) -> Option<&RegisterAllocation> {
        let mut space_needed: u32 = ty.size;

        // The last register that's allocated (inclusive).
        let mut reg_range_end = None;

        for (i, reg) in self.available_regs.iter().enumerate() {
            // All registers need to be aligned.
            // Since the list is sorted, if we
            // reach this point, no other registers
            // will work.
            if ty.align > reg.1 {
                return None;
            }

            // At this point, we're guaranteed to use this
            // register.
            // The code below is just figuring out whether this
            // will be the final register.
            self.used_regs.insert(reg.0);

            if space_needed <= reg.1 {
                reg_range_end = Some(i);
                break;
            } else {
                space_needed -= reg.1;
            }
        }

        let reg_range_end = reg_range_end?;

        // Ensure that we don't allocate beyond what's allowed.
        // Add 1 to convert from last index to length.
        if self.available_regs.len() - (reg_range_end + 1) < self.min_available as usize {
            return None;
        }

        let removed_regs = self.available_regs.drain(0..=reg_range_end).collect();
        self.sort_available_regs();

        self.variables.insert(
            name.clone(),
            RegisterAllocation {
                regs: removed_regs,
                ty,
                freed: false,
            },
        );
        self.variables.get(&name)
    }

    /// Allocates a temporary register, erroring
    /// on failure.
    ///
    /// This register must be dropped using drop_temporary.
    pub fn alloc_temporary(&mut self, size: u32) -> TemporaryRegister {
        let mut reg_idx = None;

        for (i, reg) in self.available_regs.iter().enumerate() {
            // Since the list is sorted, if we
            // reach this point, no other registers
            // will work.
            if size > reg.1 {
                panic!("Could not find a temporary register!");
            }

            if size != reg.1 {
                continue;
            }

            reg_idx = Some(i);
            break;
        }

        let Some(reg_idx) = reg_idx else {
            panic!("Could not find a temporary register!");
        };

        let reg = self.available_regs.remove(reg_idx);
        self.used_regs.insert(reg.0);

        self.sort_available_regs();

        TemporaryRegister {
            name: reg.0,
            size: reg.1,
            freed: false,
        }
    }

    /// Tries to get a previously allocated variable.
    pub fn get(&self, name: &Cow<'a, str>) -> Option<&RegisterAllocation> {
        self.variables.get(name)
    }

    /// Drops the specified variable,
    /// returning the used registers
    /// back to the available pool.
    ///
    /// Returns whether the variable existed.
    pub fn drop(&mut self, name: &Cow<'a, str>) -> bool {
        let Some(alloc) = self.variables.remove(name) else {
            return false;
        };

        self.drop_allocation(alloc);

        true
    }

    /// Drops a temporary register
    /// allocated using alloc_temporary.
    pub fn drop_temporary(&mut self, mut temporary: TemporaryRegister) {
        // Mark the allocation as freed
        // so that it doesn't error on drop.
        temporary.freed = true;

        self.available_regs.push((temporary.name, temporary.size));

        self.sort_available_regs();
    }

    /// Drops the specified allocation,
    /// returning the used registers
    /// back to the available pool.
    fn drop_allocation(&mut self, mut alloc: RegisterAllocation) {
        // Mark the allocation as freed
        // so that it doesn't error on drop.
        alloc.freed = true;

        for reg in &alloc.regs {
            self.available_regs.push(reg.clone());
        }

        self.sort_available_regs();
    }

    /// Sorts the list of available registers.
    /// Should be called after modifying it.
    /// This should also happen after updating
    /// used_regs.
    fn sort_available_regs(&mut self) {
        let used_regs = &self.used_regs;

        // Sort descending.
        // This uses stable sort
        // to preserve register order,
        // since that's done such that
        // function call registers are used
        // last.
        self.available_regs.sort_by(|a, b| {
            // Swap operands for descending
            // order.
            let cmp = b.1.cmp(&a.1);

            // This makes sure that the registers
            // which come first are those which have
            // already been used.
            // Doing so reduces the number of
            // registers required for the function.
            if cmp == Ordering::Equal {
                // We want true on the left,
                // which is descending order,
                // so swap operands.
                used_regs.contains(b.0).cmp(&used_regs.contains(a.0))
            } else {
                cmp
            }
        });
    }

    /// Tries to remove a register.
    /// The caller should call sort_available_regs sometime
    /// after calling this.
    fn remove_reg(&mut self, name: &'static str) -> Option<(&'static str, u32)> {
        let idx = self.available_regs.iter().position(|x| x.0 == name)?;
        Some(self.available_regs.remove(idx))
    }
}

/// An allocation spanning one or
/// more registers.
#[derive(Debug, Clone)]
pub struct RegisterAllocation {
    /// A list of the registers
    /// containing this allocation,
    /// along with their size (in bytes).
    ///
    /// The first item in this list
    /// represents the data that would
    /// be at the highest stack position.
    ///
    /// We are allowed to descend in size
    /// as the list goes on, but not ascend.
    ///
    /// The minimum register size is the alignment
    /// of the data being allocated.
    regs: Vec<(&'static str, u32)>,

    /// Information about the type stored
    /// in this allocation.
    ty: TypeData,

    /// Has this register been freed?
    /// This is used to report errors
    /// when an allocation is dropped
    /// without being freed.
    freed: bool,
}

impl Drop for RegisterAllocation {
    fn drop(&mut self) {
        if !self.freed {
            panic!("Dropped un-freed register allocation!");
        }
    }
}

impl RegisterAllocation {
    /// Extracts the read position for
    /// the given byte offset.
    ///
    /// This returns the register name
    /// to read from, the byte offset
    /// to read from that register,
    /// and the number of bytes available
    /// to read at that position.
    ///
    /// Due to the way a register allocation is
    /// constructed, the number of bytes
    /// read is guaranteed to be at least
    /// the alignment size of the
    /// value you're reading.
    pub fn try_read(&self, offset: u32) -> Option<(&'static str, u32, u32)> {
        let mut total_offset = 0;

        for reg in &self.regs {
            // If total_offset is 0 and reg is 2 bytes,
            // this check allows for x < 0 + 2 = offsets
            // 0 and 1, which are both within the scope
            // of the register.
            if offset < total_offset + reg.1 {
                // Checked operations are used here because this
                // code has a higher than average chance of containing
                // mistakes.
                let local_offset = offset.checked_sub(total_offset).unwrap();
                // If total_offset is 4 and reg is 2 bytes,
                // and offset is 4 byte, then local_off is
                // 0 and we have 2 bytes available.
                let bytes_available = reg.1.checked_sub(local_offset).unwrap();

                return Some((reg.0, local_offset, bytes_available));
            } else {
                total_offset += reg.1;
            }
        }

        None
    }

    /// Returns information about the type
    /// stored in this allocation.
    pub fn ty(&self) -> TypeData {
        self.ty
    }

    /// Returns the registers stored in this
    /// allocation, along with their byte size.
    pub fn regs(&self) -> impl Iterator<Item = (&'static str, u32)> {
        self.regs.iter().copied()
    }
}

/// A temporarily allocated register,
/// used for performing operations.
///
/// This will error if dropped incorrectly.
/// You must pass this back to RegisterAllocator
/// as drop_temporary.
#[derive(Debug, Eq, PartialEq)]
pub struct TemporaryRegister {
    /// The register's name.
    name: &'static str,

    /// The size (in bytes) of the register.
    size: u32,

    /// Has this register been freed?
    /// This is used to report errors
    /// when an allocation is dropped
    /// without being freed.
    freed: bool,
}

impl Drop for TemporaryRegister {
    fn drop(&mut self) {
        if !self.freed {
            panic!("Dropped un-freed temporary register allocation!");
        }
    }
}

impl TemporaryRegister {
    /// Gets the register's name.
    pub fn name(&self) -> &'static str {
        self.name
    }

    /// Gets the size (in bytes) of the register.
    pub fn bytes(&self) -> u32 {
        self.size
    }
}

/// Rounds up to make num aligned to align.
fn align_to<T: PrimInt>(num: T, align: T) -> T {
    ((num + align - T::one()) / align) * align
}
