
#include <stdlib.h>

extern void *INT,*CHAR,*BOOL,*REAL,*_STRING__,*_ARRAY__;

/*inline*/
static void copy (int *dest_p,int *source_p,int n_words)
{
	int i;

	for (i=0; i<n_words; ++i)
		dest_p[i]=source_p[i];
}

int *copy_graph_to_string (int *node_p,void *begin_free_heap,void *end_free_heap)
{
	int **stack_p,**stack_begin,**stack_end,*heap_p;

	stack_end=end_free_heap;
	stack_begin=end_free_heap;
	stack_p=end_free_heap;

	heap_p=begin_free_heap;

	if (heap_p+2>(int*)stack_begin)
		return NULL;
		
	heap_p[0]=(int)&_STRING__+2;
	heap_p[1]=0;
	heap_p+=2;

	for (;;){
		for (;;){
			int desc;
			
			desc=*node_p;
			
			if (heap_p>=(int*)stack_begin)
				return NULL;
			
			if (!(desc & 1)){
				*node_p=1+(int)heap_p;
				*heap_p++=desc;
				
				if (desc & 2){
					unsigned int arity;
					
					arity=((unsigned short *)desc)[-1];
					if (arity==0){
						if (desc==(int)&INT+2 || desc==(int)&CHAR+2 || desc==(int)&BOOL+2){
							if (heap_p>=(int*)stack_begin)
								return NULL;

							*heap_p++=node_p[1];
							break;
						} else if (desc==(int)&REAL+2){
							if (heap_p+2>(int*)stack_begin)
								return NULL;
						
							heap_p[0]=node_p[1];
							heap_p[1]=node_p[2];
							heap_p+=2;
							break;
						} else if (desc==(int)&_STRING__+2){
							unsigned int length,n_words;
							
							length=node_p[1];
							node_p+=2;
							n_words=(length+3)>>2;
							
							if (heap_p+n_words>=(int*)stack_begin)
								return NULL;
													
							*heap_p++=length;

							copy (heap_p,node_p,n_words);
							heap_p+=n_words;
							break;
						} else if (desc==(int)&_ARRAY__+2){
							int array_size,elem_desc;

							if (heap_p+2>(int*)stack_begin)
								return NULL;

							array_size=node_p[1];
							elem_desc=node_p[2];
							node_p+=3;
		
							heap_p[0]=array_size;
							heap_p[1]=elem_desc;
							heap_p+=2;
													
							if (elem_desc==0){
								stack_p-=array_size;
								if (stack_p<stack_begin){
									if ((int*)stack_p<heap_p)
										return NULL;
									stack_begin=stack_p;
								}
								
								while (--array_size>=0)
									stack_p[array_size]=(int*)node_p[array_size];						
								break;
							} else if (elem_desc==(int)&INT+2){
								if (heap_p+array_size>(int*)stack_begin)
									return NULL;
							
								copy (heap_p,node_p,array_size);	
								heap_p+=array_size;
								break;
							} else if (elem_desc==(int)&REAL+2){
								array_size<<=1;
							
								if (heap_p+array_size>(int*)stack_begin)
									return NULL;
								
								copy (heap_p,node_p,array_size);
								heap_p+=array_size;
								break;
							} else if (elem_desc==(int)&BOOL+2){
								array_size=(array_size+3)>>2;

								if (heap_p+array_size>(int*)stack_begin)
									return NULL;
								
								copy (heap_p,node_p,array_size);
								heap_p+=array_size;
								break;
							} else {
								int n_field_pointers,field_size;
								
								n_field_pointers=*(unsigned short *)elem_desc;
								field_size=((unsigned short *)elem_desc)[-1]-256;

								if (n_field_pointers==0){
									array_size*=field_size;
									
									if (heap_p+array_size>(int*)stack_begin)
										return NULL;
									
									copy (heap_p,node_p,array_size);
									heap_p+=array_size;
									break;
								} else if (n_field_pointers==field_size){
									array_size*=field_size;

									stack_p-=array_size;
									if (stack_p<stack_begin){
										if ((int*)stack_p<heap_p)
											return NULL;
										stack_begin=stack_p;
									}
									
									while (--array_size>=0)
										stack_p[array_size]=(int*)node_p[array_size];						
									break;
								} else {
									int n_non_field_pointers,n_array_pointers,n_array_non_pointers,i,*pointer_p;
									
									n_non_field_pointers=field_size-n_field_pointers;
									n_array_pointers=n_field_pointers*array_size;
									n_array_non_pointers=n_non_field_pointers*array_size;
									
									if (heap_p+n_array_non_pointers>(int*)stack_begin)
										return NULL;
									
									stack_p-=n_array_pointers;
									if (stack_p<stack_begin){
										if ((int*)stack_p<heap_p+n_array_non_pointers)
											return NULL;
										stack_begin=stack_p;
									}
									
									pointer_p=(int*)stack_p;
									
									for (i=0; i<array_size; ++i){
										copy (pointer_p,node_p,n_field_pointers);
										pointer_p+=n_field_pointers;
										node_p+=n_field_pointers;
										
										copy (heap_p,node_p,n_non_field_pointers);
										heap_p+=n_non_field_pointers;
										node_p+=n_non_field_pointers;						
									}
									break;
								}
							}
						} else {
							break;
						}				
					} else if (arity==1){
						node_p=(int*)node_p[1];
						continue;
					} else if (arity==2){
						if (stack_p<=stack_begin){
							if ((int*)stack_p<=heap_p)
								return NULL;
							--stack_begin;
						}

						*--stack_p=(int*)node_p[2];
						node_p=(int*)node_p[1];
						continue;
					} else if (arity<256){
						int **args,n_words;

						args=(int**)node_p[2];
						n_words=arity-1;
						
						stack_p-=n_words;
						if (stack_p<stack_begin){
							if ((int*)stack_p<heap_p)
								return NULL;
							stack_begin=stack_p;
						}
						
						--n_words;
						stack_p[n_words]=args[n_words];
						--n_words;
						stack_p[n_words]=args[n_words];
						while (--n_words>=0)
							stack_p[n_words]=args[n_words];

						node_p=(int*)node_p[1];
						continue;
					} else {
						int n_pointers;
						
						n_pointers=*(unsigned short*)desc;
						arity-=256;
						
						if (arity==1){
							if (n_pointers==0){
								if (heap_p>=(int*)stack_begin)
									return NULL;
						
								*heap_p++=node_p[1];
								break;
							} else {
								node_p=(int*)node_p[1];
								continue;
							}
						} else if (arity==2){
							if (n_pointers==0){
								if (heap_p+2>(int*)stack_begin)
									return NULL;
							
								heap_p[0]=node_p[1];
								heap_p[1]=node_p[2];
								heap_p+=2;
								break;
							} else {
								if (n_pointers==1){
									if (heap_p>=(int*)stack_begin)
										return NULL;
						
									*heap_p++=node_p[2];
								} else {
									if (stack_p<=stack_begin){
										if ((int*)stack_p<heap_p)
											return NULL;
										--stack_begin;
									}

									*--stack_p=(int*)node_p[2];
								}
								node_p=(int*)node_p[1];
								continue;							
							}
						} else {
							int *args;

							args=(int*)node_p[2];

							if (n_pointers==0){
								if (heap_p+arity>=(int*)stack_begin)
									return NULL;

								heap_p[0]=node_p[1];
								++heap_p;
								--arity;

								copy (heap_p,args,arity);
								heap_p+=arity;
								break;
							} else {
								int n_non_pointers;

								n_non_pointers=arity-n_pointers;

								if (n_non_pointers>0){
									int *non_pointers_p;

									if (heap_p+n_non_pointers>(int*)stack_begin)
										return NULL;

									non_pointers_p=&args[n_pointers-1];

									copy (heap_p,non_pointers_p,n_non_pointers);
									heap_p+=n_non_pointers;
								}
								
								--n_pointers;
								if (n_pointers>0){
									stack_p-=n_pointers;
									if (stack_p<stack_begin){
										if ((int*)stack_p<heap_p)
											return NULL;
										stack_begin=stack_p;
									}
									
									copy ((int*)stack_p,args,n_pointers);
								}

								node_p=(int*)node_p[1];
								continue;
							}
						}
					}
				} else {
					int arity;
					
					arity=((int*)desc)[-1];
					if (arity>1){
						if (arity<256){
							int **args,n_words;
			
							args=(int**)&node_p[2];
							n_words=arity-1;
							
							stack_p-=n_words;
							if (stack_p<stack_begin){
								if ((int*)stack_p<heap_p)
									return NULL;
								stack_begin=stack_p;
							}
							
							--n_words;
							stack_p[n_words]=args[n_words];
							while (--n_words>=0)
								stack_p[n_words]=args[n_words];
							
							node_p=(int*)node_p[1];
							continue;
						} else {
							int n_pointers,n_non_pointers,*non_pointers_p;
							
							n_non_pointers=arity>>8;
							n_pointers=(arity & 255) - n_non_pointers;
							
							if (heap_p+n_non_pointers>(int*)stack_begin)
								return NULL;
							
							non_pointers_p=&node_p[1+n_pointers];

							copy (heap_p,non_pointers_p,n_non_pointers);
							heap_p+=n_non_pointers;
							
							if (n_pointers==0)
								break;
							else {
								if (n_pointers>1){
									int **args;
									
									args=(int**)&node_p[2];
									--n_pointers;
									
									stack_p-=n_pointers;
									if (stack_p<stack_begin){
										if ((int*)stack_p<heap_p)
											return NULL;
										stack_begin=stack_p;
									}
									
									--n_pointers;
									stack_p[n_pointers]=args[n_pointers];
									while (--n_pointers>=0)
										stack_p[n_pointers]=args[n_pointers];
								}
								node_p=(int*)node_p[1];
								continue;
							}
						}
					} else if (arity==0){
						break;
					} else {
						node_p=(int*)node_p[1];
						continue;
					}
				}
			} else {
				if (heap_p>=(int*)stack_begin)
					return NULL;
				
				heap_p[0]=1+(desc-1)-(int)heap_p;
				++heap_p;
				break;
			}
		}

		if (stack_p==stack_end){
			((int*)begin_free_heap)[1]=(int)heap_p-(int)begin_free_heap-8;
			return begin_free_heap;
		}
		
		node_p=*stack_p++;
	}
	
	return NULL;
}

void remove_forwarding_pointers_from_graph (int *node_p,int **stack_end)
{
	int **stack_p;

	stack_p = stack_end;

	for (;;){
		for (;;){
			int forwarding_pointer,desc;
			
			forwarding_pointer=*node_p;
			if ((forwarding_pointer & 1)==0)
				break;
			
			desc = *((int*)(forwarding_pointer-1));
			*node_p=desc;
			
			if (desc & 2){
				unsigned int arity;
					
				arity=((unsigned short *)desc)[-1];
				if (arity==0){
					if (desc!=(int)&_ARRAY__+2){
						break;
					} else {
						int elem_desc;

						elem_desc=node_p[2];
													
						if (elem_desc==0){
							int array_size;
							
							array_size=node_p[1];
							node_p+=3;
							
							stack_p-=array_size;
							
							while (--array_size>=0)
								stack_p[array_size]=(int*)node_p[array_size];						
							break;
						} else if (elem_desc==(int)&INT+2 || elem_desc==(int)&REAL+2 || elem_desc==(int)&BOOL+2){
							break;
						} else {
							int n_field_pointers;
							
							n_field_pointers=*(unsigned short *)elem_desc;

							if (n_field_pointers!=0){
								int field_size,array_size;
								
								field_size=((unsigned short *)elem_desc)[-1]-256;

								array_size=node_p[1];
								node_p+=3;
						
								if (n_field_pointers==field_size){
									array_size*=field_size;

									stack_p-=array_size;
									
									while (--array_size>=0)
										stack_p[array_size]=(int*)node_p[array_size];						
								} else {
									int n_array_pointers,i,*pointer_p;
									
									n_array_pointers=n_field_pointers*array_size;
									
									stack_p-=n_array_pointers;
									
									pointer_p=(int*)stack_p;
									
									for (i=0; i<array_size; ++i){
										copy (pointer_p,node_p,n_field_pointers);
										pointer_p+=n_field_pointers;
										node_p+=field_size;
									}
								}
							}
							break;
						}						
					}
				} else if (arity==1){
					node_p=(int*)node_p[1];
					continue;
				} else if (arity==2){
					*--stack_p=(int*)node_p[2];
				
					node_p=(int*)node_p[1];
					continue;
				} else if (arity<256){
					int **args,n_words;

					args=(int**)node_p[2];
					n_words=arity-1;
					
					stack_p-=n_words;
					
					--n_words;
					stack_p[n_words]=args[n_words];
					while (--n_words>=0)
						stack_p[n_words]=args[n_words];

					node_p=(int*)node_p[1];
					continue;					
				} else {
					int n_pointers;
					
					n_pointers=*(unsigned short*)desc;
					if (n_pointers==0)
						break;
					else {
						if (n_pointers>=2){
							if (n_pointers==2){
								arity-=256;
								if (arity==2){
									*--stack_p=(int*)node_p[2];									
								} else {
									int **args;

									args=(int**)node_p[2];
									*--stack_p=args[0];									
								}
							} else {
								int **args,n_words;

								args=(int**)node_p[2];
								n_words=n_pointers-1;
								
								stack_p-=n_words;
								
								--n_words;
								stack_p[n_words]=args[n_words];
								while (--n_words>=0)
									stack_p[n_words]=args[n_words];

							}
						}
						node_p=(int*)node_p[1];
						continue;
					}
				}
			} else {
				int arity;

				arity=((int*)desc)[-1];
				if (arity>1){
					if (arity<256){
						int **args,n_words;
		
						args=(int**)&node_p[2];
						n_words=arity-1;
						
						stack_p-=n_words;
						
						--n_words;
						stack_p[n_words]=args[n_words];
						while (--n_words>=0)
							stack_p[n_words]=args[n_words];
						
						node_p=(int*)node_p[1];
						continue;
					} else {
						int n_pointers,n_non_pointers;
						
						n_non_pointers=arity>>8;
						n_pointers=(arity & 255) - n_non_pointers;
												
						if (n_pointers==0)
							break;
						else {
							if (n_pointers>1){
								int **args;
								
								args=(int**)&node_p[2];
								--n_pointers;
								
								stack_p-=n_pointers;
								
								--n_pointers;
								stack_p[n_pointers]=args[n_pointers];
								while (--n_pointers>=0)
									stack_p[n_pointers]=args[n_pointers];
							}
							node_p=(int*)node_p[1];
							continue;
						}						
					}
				} else if (arity==0){
					break;
				} else {
					node_p=(int*)node_p[1];
					continue;
				}
			}
		}

		if (stack_p==stack_end)
			return;
		
		node_p=*stack_p++;
	}	
}
