
/*
  TODO: implement get_num and put_num in C and use FFI to get it into
  Haskell as it seems to be the bottleneck.
 */

/* #include <errno.h> */
/* #include <stdlib.h> */
/* #include <memory.h> */
/* #include <stdio.h> */
/* #include <string.h>   */


/* static VALUE depot_write_num(FILE f, unsigned long number) */
/* { */
/*   char c,*num_buff; */
/*   int i,max; */
/*   Data_Get_Struct(self,PeaBuffer,pb);   */
/*   num      = NUM2LONG(number); */
/*   num_buff = malloc(sizeof(long)); */
/*   i=0;max=sizeof(long); */
/*   if (num < 0) { */
/*     rb_warn("Passed a negative number, write_num only does unsigned ints [%d]",num); */
/*     return self; */
/*   } */
  
/*   while((num & ~0x7F) != 0) { */
/*     sprintf(num_buff,"%c",(num & 0x7F) | 0x80);     */
/*     if(putc_internal(pb, c = *num_buff) == -1) { */
/*       flush_internal(self,pb); */
/*       putc_internal(pb,c); */
/*     } */
/*     num   >>= 7; */
/*     i++; */
/*     if(i >= max) { */
/*       /\*rb_warn("ran out of space writing number! [%d]",num);*\/ */
/*       return NULL; */
/*     } */
/*   } */
/*   sprintf(num_buff,"%c", num & 0xFF); */
/*   if(putc_internal(pb, c = *num_buff) == -1) { */
/*     flush_internal(self,pb); */
/*     putc_internal(pb,c); */
/*   } */
/*   free(num_buff); */
/*   return self; */
/* } */

/* static VALUE depot_flush(VALUE self) */
/* { */
/*   PeaBuffer *pb; */
/*   Data_Get_Struct(self,PeaBuffer,pb); */
/*   flush_internal(self,pb);   */
/*   return self; */
/* } */

/* static VALUE depot_buff_start(VALUE self) */
/* { */
/*   PeaBuffer *pb; */
/*   Data_Get_Struct(self,PeaBuffer,pb); */
/*   return INT2FIX(pb->start); */
/* } */
/* /\* */
/* static VALUE depot_buff_size(VALUE self) */
/* { */
/*   return INT2FIX(BUFF_SIZE); */
/* }  */
/* *\/ */
/* static VALUE depot_buff_pos(VALUE self) */
/* { */
/*   PeaBuffer *pb; */
/*   Data_Get_Struct(self,PeaBuffer,pb); */
/*   return INT2FIX(pb->pos); */
/* } */

/* // Reader */

/* static VALUE depot_getn(VALUE self) */
/* { */
/*   PeaBuffer *pb; */
/*   long lval; */
/*   Data_Get_Struct(self,PeaBuffer,pb); */
/*   lval = getn_internal(&self,pb); */

/*   if(lval == -1L) */
/*     return Qnil; */
/*   else */
/*     return LONG2FIX(lval);     */
/* } */
/* static long getn_internal(VALUE *self, PeaBuffer *pb) */
/* { */
/*   char c; int i,shift; long lval; */
/*   c     = getc_internal(self,pb); */
/*   shift = 0;   */
/*   if(pb->eos) { */
/*     return -1L; */
/*   } else { */
/*     lval = c & 0x7F; */
/*   } */
  
/*   while((c & 0x80) != 0) { */
/*     shift += 7; */
/*     c      = getc_internal(self,pb); */
/*     if(pb->eos) { */
/*       return -1L; */
/*     } else { */
/*       lval |= (c & 0x7F) << shift; */
/*     }     */
/*   } */
/*   return lval; */
/* } */
