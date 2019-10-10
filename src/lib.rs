use std::collections::HashMap;

struct RegAllocator {/* TODO */}

/// This trait is to ensure that LIRC itself can emit instructions for internal use. Essentially,
/// it asserts that the backend has _some_ way to do a `mov`, _some_ way to do an `add`, etc.
///
/// We will probably need to add more to this trait so that LIRC knows f.e. which register is the
/// stack pointer. Probably the name will need to be changed at that point.
pub trait HasNecessaryActions: Sized {
    fn mov() -> Self;
    fn add() -> Self;
    fn load() -> Self;
    fn store() -> Self;
}

enum RealLocation {
    Reg(asmquery::Reg),
    // Distance from where the stack pointer was when the function started.
    Stack(usize),
}

pub struct Lirc<'a, T> {
    reg_alloc: RegAllocator,
    // TODO: We could free space when a variable dies
    map: HashMap<asmquery::Var, RealLocation>,
    backend: &'a asmquery::MachineSpec<'static, T>,
    buffer: Vec<u8>,
}

pub enum LircDirective<T> {
    Action(asmquery::Action<T>),
    CallingConvention(/* TODO */),
    DefineLabel(/* TODO */),
    // .. etc ...
}

pub enum LircError {
    NoMatchingInstrs,
    // TODO
}

// TODO: This currently doesn't have any way to handle shuffling elements around - for example,
//       if an element is on the Wasm stack we need to know where it is but it doesn't need to
//       stay in the same place. If an element is in a variable (i.e. is currently in use for
//       some assembly that is currently being emitted) we cannot.
impl<T> Lirc<'_, T>
where
    T: PartialEq,
{
    pub fn compile<I>(&mut self, instrs: I) -> Result<(), LircError>
    where
        I: IntoIterator<Item = LircDirective<T>>,
    {
        use smallbitvec::SmallBitVec;

        struct ActionState {
            possible_mask: SmallBitVec,
            candidate_instr: usize,
        }

        let mut instrs = itertools::put_back(instrs);
        // We will set this to a `Some` when we're still able to consume `Action`s. This
        // means that we have the necessary control to, for example, allow the definition
        // of calling conventions in the middle of a stream of `Actions` but _disallow_
        // defining labels in the middle of a stream, as the former doesn't affect the
        // coalescing of actions whereas the latter, of course, does.
        let mut action_state: Option<ActionState> = None;

        while let Some(directive) = instrs.next() {
            match directive {
                LircDirective::Action(action) => {
                    // TODO: We can precalculate/memoise this calculation here
                    let mask: SmallBitVec = self
                        .backend
                        .instrs_iter()
                        .map(|idef| {
                            idef.actions()
                                .any(|idef_action| idef_action.action == action.action)
                        })
                        .collect();

                    let mask = if let Some(action_state) = &action_state {
                        // TODO: There should be a far faster version of this that just `&`s the
                        //       elements together in bulk but it's not implemented in the `smallbitvec`
                        //       crate yet.
                        mask.iter()
                            .zip(&action_state.possible_mask)
                            .map(|(a, b)| a && b)
                            .collect()
                    } else {
                        mask
                    };

                    // TODO: We should have a fast path for the case that the mask is all false, since
                    //       checking if a bitvec is all false is far faster than itering through each
                    //       bit checking it in turn.
                    let remaining_candidate_instructions = self
                        .backend
                        .instrs_iter()
                        .enumerate()
                        .filter(|(i, _)| mask[*i])
                        .map(|(_, v)| v);

                    let available_instr =
                        remaining_candidate_instructions.enumerate().find(|instr| {
                            // TODO: Check each instruction and return true if the data flow matches what
                            //       we need, etc.
                            unimplemented!()
                        });

                    if let Some((available_instr_id, _)) = available_instr {
                        action_state = Some(ActionState {
                            possible_mask: mask,
                            candidate_instr: available_instr_id,
                        });
                    } else {
                        // If `action_state` is none we return an error, otherwise we continue to
                        // emitting the instruction.
                        let action_state =
                            action_state.take().ok_or(LircError::NoMatchingInstrs)?;

                        // This is where we actually emit the instruction - if there are no candidate
                        // instructions for the combination of the previous actions plus this one then
                        // we know that we must emit the current pending instruction before continuing
                        // on.

                        instrs.put_back(LircDirective::Action(action));
                    }
                }
                _ => unimplemented!(),
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
