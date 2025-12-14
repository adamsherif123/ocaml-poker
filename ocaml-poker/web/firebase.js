// web/firebase.js (ES module)

import { initializeApp } from "https://www.gstatic.com/firebasejs/10.12.5/firebase-app.js";
import {
  getAuth,
  onAuthStateChanged,
  createUserWithEmailAndPassword,
  signInWithEmailAndPassword,
  signOut,
} from "https://www.gstatic.com/firebasejs/10.12.5/firebase-auth.js";

import {
  getFirestore,
  doc,
  getDoc,
  setDoc,
  updateDoc,
  serverTimestamp,
} from "https://www.gstatic.com/firebasejs/10.12.5/firebase-firestore.js";

const firebaseConfig = {
  apiKey: "AIzaSyCL3oOF6q2X_jZ8QmUKfgucnG0ciJsKWSg",
  authDomain: "poker-8100a.firebaseapp.com",
  projectId: "poker-8100a",
  storageBucket: "poker-8100a.firebasestorage.app",
  messagingSenderId: "817683902060",
  appId: "1:817683902060:web:a6d6a8fab9c6ac1fb6b249"
};

const app = initializeApp(firebaseConfig);
const auth = getAuth(app);
const db = getFirestore(app);

const $ = (id) => document.getElementById(id);
const show = (id, on) => { const el = $(id); if (el) el.style.display = on ? "" : "none"; };

function setAuthedUI({ name = "", email = "", chips = 0 } = {}) {
  const n = $("authedName"); if (n) n.textContent = name;
  const e = $("authedEmail"); if (e) e.textContent = email;
  const c = $("authedChips"); if (c) c.textContent = `${chips} chips`;
}
const setMsg = (s) => { const el = $("msg"); if (el) el.textContent = s || ""; };

async function ensureUserDoc(user, maybeNameFromSignup = null) {
  const ref = doc(db, "users", user.uid);
  const snap = await getDoc(ref);

  if (!snap.exists()) {
    const name =
      (maybeNameFromSignup && maybeNameFromSignup.trim()) ||
      (user.email ? user.email.split("@")[0] : "Player 1");

    await setDoc(ref, {
      email: user.email || "",
      name,
      chips: 1000,
      createdAt: serverTimestamp(),
      updatedAt: serverTimestamp(),
    });

    return { name, chips: 1000 };
  }

  const data = snap.data() || {};
  let needsUpdate = false;
  const patch = { updatedAt: serverTimestamp() };

  if (typeof data.chips !== "number") { patch.chips = 1000; needsUpdate = true; }
  if (!data.name || typeof data.name !== "string") {
    patch.name = (user.email ? user.email.split("@")[0] : "Player 1");
    needsUpdate = true;
  }

  if (needsUpdate) await setDoc(ref, patch, { merge: true });

  const snap2 = await getDoc(ref);
  const data2 = snap2.data() || {};
  return {
    name: data2.name || "Player 1",
    chips: typeof data2.chips === "number" ? data2.chips : 1000,
  };
}

// Expose functions for OCaml to call
window.fbSaveChips = async (chips) => {
  const u = auth.currentUser;
  if (!u) return;
  const ref = doc(db, "users", u.uid);
  await updateDoc(ref, { chips: Number(chips), updatedAt: serverTimestamp() });
};

window.fbSaveName = async (name) => {
  const u = auth.currentUser;
  if (!u) return;
  const ref = doc(db, "users", u.uid);
  await updateDoc(ref, { name: String(name), updatedAt: serverTimestamp() });
};

async function callOcamlSetUser(uid, email, chips, name) {
  // app.bc.js defines these
  if (typeof window.ocamlSetUser === "function") {
    window.ocamlSetUser(uid, email, chips, name);
  }
}

function callOcamlClearUser() {
  if (typeof window.ocamlClearUser === "function") {
    window.ocamlClearUser();
  }
}

// UI button handlers
$("btnSignup")?.addEventListener("click", async () => {
  try {
    setMsg("");
    const name = ($("signupName")?.value || "").trim();
    const email = ($("signupEmail")?.value || "").trim();
    const pass = $("signupPass")?.value || "";
    if (!name) return setMsg("Please enter your name for sign up.");
    if (!email || !pass) return setMsg("Email + password required.");

    const cred = await createUserWithEmailAndPassword(auth, email, pass);
    await ensureUserDoc(cred.user, name);
    setMsg("Account created ✅");
  } catch (e) {
    setMsg(e?.message || String(e));
  }
});

$("btnLogin")?.addEventListener("click", async () => {
  try {
    setMsg("");
    const email = ($("loginEmail")?.value || "").trim();
    const pass = $("loginPass")?.value || "";
    if (!email || !pass) return setMsg("Email + password required.");
    await signInWithEmailAndPassword(auth, email, pass);
    setMsg("");
  } catch (e) {
    setMsg(e?.message || String(e));
  }
});

$("btnLogout")?.addEventListener("click", async () => {
  try {
    await signOut(auth);
    setMsg("Signed out.");
  } catch (e) {
    setMsg(e?.message || String(e));
  }
});

// Auth listener -> load Firestore user doc -> notify OCaml
onAuthStateChanged(auth, async (user) => {
  if (!user) {
    // show login/signup, hide authed panel
    show("authForms", true);
    show("authAuthed", false);
    setAuthedUI({ name: "", email: "", chips: 0 });

    callOcamlClearUser();
    return;
  }

  // hide login/signup, show authed panel
  show("authForms", false);
  show("authAuthed", true);

  try {
    const { name, chips } = await ensureUserDoc(user, null);
    setAuthedUI({ name, email: user.email || "", chips });

    await callOcamlSetUser(user.uid, user.email || "", chips, name);
    setMsg("");

    // optional: clear form inputs when signed in
    if ($("loginPass")) $("loginPass").value = "";
    if ($("signupPass")) $("signupPass").value = "";
  } catch (e) {
    setMsg(e?.message || String(e));
  }
});

