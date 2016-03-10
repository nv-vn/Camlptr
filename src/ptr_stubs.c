#include <stdlib.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>

CAMLprim value deref_stub(value addr) {
  long addr_ = addr;
  int *ptr;
  ptr = (int *) addr_;
  return Val_int(*ptr);
}

CAMLprim value assign_stub(value addr, value val) {
  long addr_ = addr;
  int val_ = Int_val(val);
  int *ptr;
  ptr = (int *) addr_;
  *ptr = val_;
  return Val_unit;
}

CAMLprim value offset_stub(value addr, value off) {
  long addr_ = addr;
  long off_ = Int64_val(off);
  long result = addr_ + off_;
  return result;
}

CAMLprim value alloc_stub(value size) {
  int size_ = Int_val(size);
  void *ptr = malloc(size_);
  return (long) ptr;
}

CAMLprim value free_stub(value addr) {
  long addr_ = addr;
  free((void *) addr_);

  return Val_unit;
}

CAMLprim value getref_stub(value ptr) {
  int val = Int_val(ptr);
  long addr_ = (long) &val;
  return addr_;
}

CAMLprim value puts_stub(value addr) {
  long addr_ = addr;
  char *str = (char *) addr_;
  puts(str);
  return Val_unit;
}
