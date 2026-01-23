# Agent guide

Before you write any code, read:

- The **why** document: `src/index.vto`  
  (there might be a compiled version `dist/index.html`, if so prefer that)
- The **how** document: `docs/ARCHITECTURE.md`


## Your responsibilities

As an agent, your job is to:
- Keep the architecture described in `docs/ARCHITECTURE.md` intact.
- Preserve the overall goals and constraints described in the 'why' document.

If you are unsure how to implement something:
- Prefer a small, clean, testable implementation that matches the intent.
- Do **not** invent new architecture without clear justification in comments.
