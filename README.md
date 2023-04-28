Normales Kompilieren:

cd hakyll
stack exec site build
stack exec site watch

---

If you made changes to site.hs, you need to recompile site.hs followed by a rebuild:

stack build
stack exec site rebuild

---

stack exec site clean removes these directories, and stack exec site rebuild performs a clean and then a build.

---

