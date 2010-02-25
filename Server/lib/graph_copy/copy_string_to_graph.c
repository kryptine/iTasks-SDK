
#include <stdlib.h>

extern void *INT,*CHAR,*BOOL,*REAL,*_STRING__,*_ARRAY__;
extern int small_integers[],static_characters[];

inline static void copy (int *dest_p,int *source_p,int n_words)
{
	int i;

	for (i=0; i<n_words; ++i)
		dest_p[i]=source_p[i];
}

int *copy_string_to_graph (int *string_p,void *begin_free_heap,void *end_free_heap,int **last_heap_pa)
{
	int ***stack_p,***stack_begin,***stack_end,*heap_p,**arg_a,*root_node_p,n_free_words;

	string_p+=2;

	stack_end=end_free_heap;
	stack_begin=end_free_heap;
	stack_p=end_free_heap;
		
	heap_p=begin_free_heap;
	
	n_free_words=(int*)end_free_heap-(int*)begin_free_heap;
	arg_a=&root_node_p;

	for (;;){
		for (;;){
			int desc;
			
			desc=*string_p;
			
			if (--n_free_words<0){
				*last_heap_pa=heap_p+1+(stack_end-stack_begin);
				return (int*)((int)string_p+1);
			}

			if (!(desc & 1)){
				*string_p=(int)heap_p;
				*arg_a=heap_p;
				*heap_p=desc;
				if (desc & 2){
					unsigned int arity;
					
					arity=((unsigned short *)desc)[-1];
					if (arity==0){
						if (desc==(int)&INT+2){
							int i;
							
							i=string_p[1];
							if ((unsigned)i<=(unsigned)32){
								int *a;
								
								a=&small_integers[i<<1];
								++n_free_words;
								*arg_a=a;
								*string_p=(int)a;
							} else {
								if (--n_free_words<0){
									*last_heap_pa=heap_p+2+(stack_end-stack_begin);
									return (int*)((int)&string_p[1]+1);
								}
								
								heap_p[1]=i;
								heap_p+=2;
							}
							string_p+=2;
							break;
						} else if (desc==(int)&CHAR+2){
							unsigned char c;
							int *a;
							
							c=(unsigned char)(string_p[1]);
							a=&static_characters[c<<1];
							++n_free_words;
							*arg_a=a;
							*string_p=(int)a;
							string_p+=2;
							break;
						} else if (desc==(int)&BOOL+2){
							if (--n_free_words<0){
								*last_heap_pa=heap_p+2+(stack_end-stack_begin);
								return (int*)((int)&string_p[1]+1);
							}
							
							heap_p[1]=string_p[1];
							string_p+=2;
							heap_p+=2;
							break;
						} else if (desc==(int)&REAL+2){
							n_free_words-=2;
							if (n_free_words<0){
								*last_heap_pa=heap_p+3+(stack_end-stack_begin);
								return (int*)((int)&string_p[1]+1);
							}
							
							heap_p[1]=string_p[1];
							heap_p[2]=string_p[2];
							string_p+=3;
							heap_p+=3;
							break;
						} else if (desc==(int)&_STRING__+2){
							unsigned int length,n_words;
							
							length=string_p[1];
							string_p+=2;
							n_words=(length+3)>>2;
							
							n_free_words-=n_words+1;
							if (n_free_words<0){
								*last_heap_pa=heap_p+2+(stack_end-stack_begin);
								return (int*)((int)string_p+1);
							}
													
							heap_p[1]=length;
							heap_p+=2;

							copy (heap_p,string_p,n_words);
							string_p+=n_words;
							heap_p+=n_words;
							break;
						} else if (desc==(int)&_ARRAY__+2){
							int array_size,elem_desc;

							n_free_words-=2;
							if (n_free_words<0){
								*last_heap_pa=heap_p+3+(stack_end-stack_begin);
								return (int*)((int)&string_p[1]+1);
							}
							
							array_size=string_p[1];
							elem_desc=string_p[2];
							string_p+=3;
		
							heap_p[1]=array_size;
							heap_p[2]=elem_desc;
							heap_p+=3;
													
							if (elem_desc==0){
								int i;
								
								stack_p-=array_size;
								if (stack_p<stack_begin){
									int extra_words;

									extra_words=stack_begin-stack_p;
									n_free_words-=extra_words;
									if (n_free_words<0){
										*last_heap_pa=heap_p+array_size+(stack_end-stack_p);
										return (int*)((int)string_p+1);
									}
									stack_begin=stack_p;
								}
								
								i=array_size;
								while (--i>=0)
									stack_p[i]=(int**)&heap_p[i];

								heap_p+=array_size;
								break;
							} else if (elem_desc==(int)&INT+2){
								n_free_words-=array_size;
								if (n_free_words<0){
									*last_heap_pa=heap_p+array_size+(stack_end-stack_begin);
									return (int*)((int)string_p+1);
								}

								copy (heap_p,string_p,array_size);
								string_p+=array_size;
								heap_p+=array_size;
								break;
							} else if (elem_desc==(int)&REAL+2){
								array_size<<=1;
							
								n_free_words-=array_size;
								if (n_free_words<0){
									*last_heap_pa=heap_p+array_size+(stack_end-stack_begin);
									return (int*)((int)string_p+1);
								}
								
								copy (heap_p,string_p,array_size);
								string_p+=array_size;
								heap_p+=array_size;
								break;
							} else if (elem_desc==(int)&BOOL+2){
								array_size=(array_size+3)>>2;

								n_free_words-=array_size;
								if (n_free_words<0){
									*last_heap_pa=heap_p+array_size+(stack_end-stack_begin);
									return (int*)((int)string_p+1);
								}
								
								copy (heap_p,string_p,array_size);
								string_p+=array_size;
								heap_p+=array_size;
								break;
							} else {
								int n_field_pointers,field_size;

								n_field_pointers=*(unsigned short *)elem_desc;
								field_size=((unsigned short *)elem_desc)[-1]-256;

								if (n_field_pointers==0){
									array_size*=field_size;
									
									n_free_words-=array_size;
									if (n_free_words<0){
										*last_heap_pa=heap_p+array_size+(stack_end-stack_begin);
										return (int*)((int)string_p+1);
									}
																		
									copy (heap_p,string_p,array_size);
									string_p+=array_size;
									heap_p+=array_size;
									break;
								} else if (n_field_pointers==field_size){
									int i;
									
									array_size*=field_size;

									stack_p-=array_size;
									if (stack_p<stack_begin){
										int extra_words;

										extra_words=stack_begin-stack_p;
										n_free_words-=extra_words;
										if (n_free_words<0){
											*last_heap_pa=heap_p+array_size+(stack_end-stack_p);
											return (int*)((int)string_p+1);
										}
										stack_begin=stack_p;
									}
									
									i=array_size;
									while (--i>=0)
										stack_p[i]=(int**)&heap_p[i];
									
									heap_p+=array_size;
									break;
								} else {
									int n_non_field_pointers,i,***pointer_p;
									
									n_non_field_pointers=field_size-n_field_pointers;
									
									n_free_words-=array_size*field_size;
									if (n_free_words<0){
										*last_heap_pa=heap_p+array_size*field_size+(stack_end-stack_begin);
										return (int*)((int)string_p+1);
									}
									
									stack_p-=array_size*n_field_pointers;
									if (stack_p<stack_begin){
										int extra_words;

										extra_words=stack_begin-stack_p;
										n_free_words-=extra_words;
										if (n_free_words<0){
											*last_heap_pa=heap_p+(stack_end-stack_p);
											return (int*)((int)string_p+1);
										}
										stack_begin=stack_p;
									}
									
									pointer_p=stack_p;
									
									for (i=0; i<array_size; ++i){
										int n;
										
										n=n_field_pointers;
										while (--n>=0)
											pointer_p[n]=(int**)&heap_p[n];
										pointer_p+=n_field_pointers;
										heap_p+=n_field_pointers;
										
										copy (heap_p,string_p,n_non_field_pointers);
										heap_p+=n_non_field_pointers;
										string_p+=n_non_field_pointers;						
									}
									break;
								}
							}
						} else {
#ifdef OLD_DESCRIPTORS
							desc-=10;
#else
							desc-=6;							
#endif
							++n_free_words;
							*arg_a=(int*)desc;
							*string_p=desc;
							++string_p;
							break;
						}
					} else if (arity==1){
						if (--n_free_words<0){
							*last_heap_pa=heap_p+2+(stack_end-stack_begin);
							return (int*)((int)&string_p[1]+1);
						}
						arg_a=(int**)&heap_p[1];
						++string_p;
						heap_p+=2;
						continue;
					} else if (arity==2){
						n_free_words-=2;
						if (n_free_words<0){
							*last_heap_pa=heap_p+3+(stack_end-stack_begin);
							return (int*)((int)&string_p[1]+1);
						}
						
						if (stack_p<=stack_begin){
							if (--n_free_words<0){
								*last_heap_pa=heap_p+3+(stack_end-1-stack_begin);
								return (int*)((int)&string_p[1]+1);
							}
							--stack_begin;
						}
						
						*--stack_p=(int**)&heap_p[2];
						arg_a=(int**)&heap_p[1];
						++string_p;
						heap_p+=3;
						continue;
					} else if (arity<256){
						int n_words;

						n_free_words-=arity+1;
						if (n_free_words<0){
							*last_heap_pa=heap_p+arity+2+(stack_end-stack_begin);
							return (int*)((int)&string_p[1]+1);
						}

						arg_a=(int**)&heap_p[1];
						heap_p[2]=(int)&heap_p[3];
						heap_p+=3;

						n_words=arity-1;

						stack_p-=n_words;
						if (stack_p<stack_begin){
							int extra_words;

							extra_words=stack_begin-stack_p;
							n_free_words-=extra_words;
							if (n_free_words<0){
								*last_heap_pa=heap_p+arity-1+(stack_end-stack_begin);
								return (int*)((int)&string_p[1]+1);
							}
							stack_begin=stack_p;
						}

						--n_words;
						stack_p[n_words]=(int**)&heap_p[n_words];
						--n_words;
						stack_p[n_words]=(int**)&heap_p[n_words];
						while (--n_words>=0)
							stack_p[n_words]=(int**)&heap_p[n_words];

						heap_p+=arity-1;
						++string_p;
						continue;
					} else {
						int n_pointers;
						
						n_pointers=*(unsigned short*)desc;
						arity-=256;
						
						if (arity==1){
							if (--n_free_words<0){
								*last_heap_pa=heap_p+2+(stack_end-stack_begin);
								return (int*)((int)&string_p[1]+1);
							}
							
							if (n_pointers==0){						
								heap_p[1]=string_p[1];
								string_p+=2;
								heap_p+=2;
								break;
							} else {
								arg_a=(int**)&heap_p[1];
								++string_p;
								heap_p+=2;
								continue;
							}
						} else if (arity==2){
							n_free_words-=2;
							if (n_free_words<0){
								*last_heap_pa=heap_p+3+(stack_end-stack_begin);
								return (int*)((int)&string_p[1]+1);
							}

							if (n_pointers==0){
								heap_p[1]=string_p[1];
								heap_p[2]=string_p[2];
								string_p+=3;
								heap_p+=3;
								break;
							} else {
								if (n_pointers==1){
									heap_p[2]=string_p[1];
									string_p+=2;
								} else {
									if (stack_p<=stack_begin){
										if (--n_free_words<0){
											*last_heap_pa=heap_p+3+1+(stack_end-stack_begin);
											return (int*)((int)&string_p[1]+1);
										}
										--stack_begin;
									}
									++string_p;
									*--stack_p=(int**)&heap_p[2];
								}
								arg_a=(int**)&heap_p[1];
								heap_p+=3;
								continue;							
							}
						} else {
							n_free_words-=arity+1;
							if (n_free_words<0){
								*last_heap_pa=heap_p+arity+(stack_end-stack_begin);
								return (int*)((int)&string_p[1]+1);
							}
							
							heap_p[2]=(int)&heap_p[3];
							
							if (n_pointers==0){
								heap_p[1]=string_p[1];
								heap_p+=3;
								string_p+=2;
								--arity;
								copy (heap_p,string_p,arity);
								string_p+=arity;
								heap_p+=arity;
								break;
							} else {
								int n_non_pointers;

								arg_a=(int**)&heap_p[1];
								heap_p+=3;

								n_non_pointers=arity-n_pointers;
								++string_p;

								if (n_non_pointers>0){
									int *non_pointers_p;

									non_pointers_p=&heap_p[n_pointers-1];

									copy (non_pointers_p,string_p,n_non_pointers);
									string_p+=n_non_pointers;
								}
								
								--n_pointers;
								if (n_pointers>0){
									int i;
									
									stack_p-=n_pointers;
									if (stack_p<stack_begin){
										int extra_words;

										extra_words=stack_begin-stack_p;
										n_free_words-=extra_words;
										if (n_free_words<0){
											*last_heap_pa=heap_p+n_pointers+n_non_pointers+(stack_end-stack_p);
											return (int*)((int)string_p+1);
										}
										stack_begin=stack_p;
									}
									
									i=n_pointers;
									while (--i>=0)
										stack_p[i]=(int**)&heap_p[i];
								}
								heap_p+=n_pointers+n_non_pointers;
								continue;
							}
						}
					}
				} else {
					int arity;

					arity=((int*)desc)[-1];
					if (arity>1){
						if (arity<256){
							int n_words;
							
							n_free_words-=arity;
							if (n_free_words<0){
								*last_heap_pa=heap_p+arity+1+(stack_end-stack_begin);
								return (int*)((int)&string_p[1]+1);
							}

							n_words=arity-1;

							stack_p-=n_words;
							if (stack_p<stack_begin){
								int extra_words;

								extra_words=stack_begin-stack_p;
								n_free_words-=extra_words;
								if (n_free_words<0){
									*last_heap_pa=heap_p+arity+1+(stack_end-stack_p);
									return (int*)((int)&string_p[1]+1);
								}
								stack_begin=stack_p;
							}

							arg_a=(int**)&heap_p[1];
							heap_p+=2;
							
							--n_words;
							stack_p[n_words]=(int**)&heap_p[n_words];
							while (--n_words>=0)
								stack_p[n_words]=(int**)&heap_p[n_words];

							++string_p;
							heap_p+=arity-1;
							continue;							
						} else {
							int n_pointers,n_non_pointers,*non_pointers_p;
							
							n_non_pointers=arity>>8;
							arity=arity & 255;
							n_pointers=arity - n_non_pointers;

							n_free_words-=arity;
							if (n_free_words<0){
								*last_heap_pa=heap_p+arity+1+(stack_end-stack_begin);
								return (int*)((int)&string_p[1]+1);
							}

							++string_p;
							++heap_p;
							non_pointers_p=&heap_p[n_pointers];

							copy (non_pointers_p,string_p,n_non_pointers);
							string_p+=n_non_pointers;
							
							if (n_pointers==0){
								heap_p+=arity;
								break;
							} else {
								arg_a=(int**)&heap_p[0];
								++heap_p;
								if (n_pointers>1){
									--n_pointers;
									
									stack_p-=n_pointers;
									if (stack_p<stack_begin){
										int extra_words;

										extra_words=stack_begin-stack_p;
										n_free_words-=extra_words;
										if (n_free_words<0){
											*last_heap_pa=heap_p+arity+(stack_end-stack_p);
											return (int*)((int)string_p+1);
										}
										stack_begin=stack_p;
									}
									
									--n_pointers;
									stack_p[n_pointers]=(int**)&heap_p[n_pointers];
									while (--n_pointers>=0)
										stack_p[n_pointers]=(int**)&heap_p[n_pointers];
								}
								heap_p+=arity-1;
								continue;
							}
						}
					} else {
						n_free_words-=2;
						if (n_free_words<0){
							*last_heap_pa=heap_p+3+(stack_end-stack_begin);
							return (int*)((int)&string_p[1]+1);
						}
					
						++string_p;
						if (arity==0){
							heap_p+=3;
							break;
						} else {
							arg_a=(int**)&heap_p[1];
							heap_p+=3;
							continue;
						}
					}
				}
			} else {
				int *node_p;
				
				node_p=*(int**)((int)string_p+(desc-1));
				*arg_a=node_p;
				++string_p;
				break;
			}
		}

		if (stack_p==stack_end)
			break;
		
		arg_a=*stack_p++;
	}
	
	*last_heap_pa=heap_p;
	return root_node_p;
}

void remove_forwarding_pointers_from_string (int *string_p,int *end_forwarding_pointers)
{
	string_p+=2;

	while (string_p<end_forwarding_pointers){
		int forwarding_pointer;
			
		forwarding_pointer=*string_p;
		if (!(forwarding_pointer & 1)){
			int desc;
			
			desc=*(int*)forwarding_pointer;
			*string_p=desc;
			if (desc & 2){
				unsigned int arity;
				
				arity=((unsigned short *)desc)[-1];
				if (arity==0){
					if (desc==(int)&INT+2 || desc==(int)&CHAR+2 || desc==(int)&BOOL+2){
						string_p+=2;
					} else if (desc==(int)&REAL+2){
						string_p+=3;
					} else if (desc==(int)&_STRING__+2){
						unsigned int length,n_words;
							
						length=string_p[1];
						string_p+=2;
						n_words=(length+3)>>2;
						string_p+=n_words;
					} else if (desc==(int)&_ARRAY__+2){
						int array_size,elem_desc;

						array_size=string_p[1];
						elem_desc=string_p[2];
						string_p+=3;
													
						if (elem_desc==0){
						} else if (elem_desc==(int)&INT+2){
							string_p+=array_size;
						} else if (elem_desc==(int)&REAL+2){
							array_size<<=1;
							string_p+=array_size;
						} else if (elem_desc==(int)&BOOL+2){
							array_size=(array_size+3)>>2;
							string_p+=array_size;
						} else {
							int n_field_pointers,n_non_field_pointers,field_size;

							n_field_pointers=*(unsigned short *)elem_desc;
							field_size=((unsigned short *)elem_desc)[-1]-256;
							n_non_field_pointers=field_size-n_field_pointers;
						
							string_p+=n_non_field_pointers*array_size;
						}
					} else {
						++string_p;
					}
				} else {
					++string_p;
					if (arity>=256){
						int n_pointers,n_non_pointers;

						n_pointers=*(unsigned short*)desc;
						arity-=256;
						n_non_pointers=arity-n_pointers;
						string_p+=n_non_pointers;
					}
				}
			} else {
				int arity;

				arity=((int*)desc)[-1];
				++string_p;
				if (arity>=256){
					int n_non_pointers;
					
					n_non_pointers=arity>>8;
					string_p+=n_non_pointers;
				}
			}
		} else {
			++string_p;
		}
	}
}
