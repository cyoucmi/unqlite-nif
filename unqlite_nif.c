#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "unqlite/unqlite.h"
#include "erl_nif.h"

#define MAX_PATH 256

static ErlNifResourceType *UnqliteResourceType; 

static unqlite *pDb;


static void
UnqliteResourceDtor(ErlNifEnv *env, void *obj){
    struct unqlite_kv_cursor **cursor = obj;
    unqlite_kv_cursor_release(pDb, *cursor);
}

/*加载unqlite_nif*/
static int unqlite_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);

/*卸载unqlite_nif*/
static void unqlite_unload(ErlNifEnv *env, void *priv_data);

static ERL_NIF_TERM unqlite_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM
make_fatal_tuple_info(unqlite *pDb, const char *zMsg, ErlNifEnv *env){
    const char *zErr = NULL;
    if (pDb){
        int iLen = 0;
        unqlite_config(pDb, UNQLITE_CONFIG_ERR_LOG, &zErr, &iLen);
    }else{
        if(zMsg) {
            zErr = zMsg;
        }
    }
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, zErr, ERL_NIF_LATIN1));
}


static int
unqlite_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info){
    pDb = NULL;
    ErlNifResourceFlags tried;
    UnqliteResourceType = enif_open_resource_type(env, NULL, "unqlite", UnqliteResourceDtor, ERL_NIF_RT_CREATE,&tried);
   return 0;
};


static void 
unqlite_unload(ErlNifEnv *env, void *priv_data){
    unqlite_close(pDb);
}


static ERL_NIF_TERM 
unqlite_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){

    char buf[MAX_PATH];
    size_t buf_size = MAX_PATH;
    if( enif_get_atom(env, argv[0], buf, buf_size, ERL_NIF_LATIN1) <= 0){/*获取失败*/
        strcpy(buf, "unqlite_test.db");/**读取默认值*/
    }    

	/* Open our database */
	int rc = unqlite_open(&pDb, buf, UNQLITE_OPEN_CREATE);
	if( rc != UNQLITE_OK ){
		return make_fatal_tuple_info(0,"Out of memory", env);
	}
   return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM 
unqlite_set(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    ErlNifBinary key_bin;
    ErlNifBinary value_bin;
    enif_inspect_binary(env, argv[0], &key_bin);
    enif_inspect_binary(env, argv[1], &value_bin);
	int rc = unqlite_kv_store(pDb, key_bin.data, key_bin.size, value_bin.data, value_bin.size); 
	if( rc != UNQLITE_OK ){
		return make_fatal_tuple_info(pDb, 0, env);
	}
   return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM 
unqlite_get(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    ErlNifBinary key_bin;
    unqlite_int64 nBytes;  //Data length
    enif_inspect_binary(env, argv[0], &key_bin);
    int rc = unqlite_kv_fetch(pDb,key_bin.data,key_bin.size,NULL,&nBytes);
	if( rc != UNQLITE_OK ){
        if( rc == UNQLITE_NOTFOUND){
            return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "not_found"));
        }else{
            return make_fatal_tuple_info(pDb, 0, env);
        }
	}
    ErlNifBinary value_bin;
    if (!enif_alloc_binary(nBytes, &value_bin)) { 
	return enif_make_badarg(env);
    }
    unqlite_kv_fetch(pDb,key_bin.data,key_bin.size,value_bin.data,&nBytes);
    return enif_make_binary(env, &value_bin);
}

static ERL_NIF_TERM
unqlite_del(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    ErlNifBinary key_bin; 
    enif_inspect_binary(env, argv[0], &key_bin);
    int rc = unqlite_kv_delete(pDb, key_bin.data, key_bin.size);
	if( rc != UNQLITE_OK ){
        if( rc == UNQLITE_NOTFOUND){
            return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "not_found"));
        }else{
            return make_fatal_tuple_info(pDb, 0, env);
        }
	}
   return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
unqlite_commit1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    int rc = unqlite_commit(pDb);
	if( rc != UNQLITE_OK ){
        return make_fatal_tuple_info(pDb, 0, env);
	}
   return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
unqlite_rollback1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    int rc = unqlite_rollback(pDb);
	if( rc != UNQLITE_OK ){
        return make_fatal_tuple_info(pDb, 0, env);
	}
   return enif_make_atom(env, "ok");
}


static ERL_NIF_TERM
unqlite_init_cursor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    struct unqlite_kv_cursor **pp_cursor = (struct unqlite_kv_cursor**)enif_alloc_resource(UnqliteResourceType, sizeof(unqlite_kv_cursor*));
    int rc = unqlite_kv_cursor_init(pDb, pp_cursor);
	if( rc != UNQLITE_OK ){
        return make_fatal_tuple_info(pDb, 0, env);
	}
   ERL_NIF_TERM ret = enif_make_resource(env, (void*)pp_cursor);
   enif_release_resource(pp_cursor);
   return ret;
}

static ERL_NIF_TERM
unqlite_cursor_first_entry(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    struct unqlite_kv_cursor **pp_cursor = NULL;
    if (!enif_get_resource(env, argv[0], UnqliteResourceType, (void**)&pp_cursor)) { 
	    return enif_make_badarg(env);
    }
    int rc = unqlite_kv_cursor_first_entry(*pp_cursor);
	if( rc != UNQLITE_OK ){
        return make_fatal_tuple_info(pDb, 0, env);
	}
   return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
unqlite_cursor_last_entry(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    struct unqlite_kv_cursor **pp_cursor = NULL;
    if (!enif_get_resource(env, argv[0], UnqliteResourceType, (void**)&pp_cursor)) { 
	    return enif_make_badarg(env);
    }
    int rc = unqlite_kv_cursor_last_entry(*pp_cursor);
	if( rc != UNQLITE_OK ){
        return make_fatal_tuple_info(pDb, 0, env);
	}
   return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
unqlite_cursor_valid_entry(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    struct unqlite_kv_cursor **pp_cursor = NULL;
    if (!enif_get_resource(env, argv[0], UnqliteResourceType, (void**)&pp_cursor)) { 
	    return enif_make_badarg(env);
    }
    int rc = unqlite_kv_cursor_valid_entry(*pp_cursor);
	if( rc ){
        return enif_make_atom(env, "true");
	}else{
        return enif_make_atom(env, "false");
    }
}

static ERL_NIF_TERM
unqlite_cursor_next_entry(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    struct unqlite_kv_cursor **pp_cursor = NULL;
    if (!enif_get_resource(env, argv[0], UnqliteResourceType, (void**)&pp_cursor)) { 
	    return enif_make_badarg(env);
    }
    int rc = unqlite_kv_cursor_next_entry(*pp_cursor);
	if( rc != UNQLITE_OK ){
        return make_fatal_tuple_info(pDb, 0, env);
	}
   return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
unqlite_cursor_prev_entry(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    struct unqlite_kv_cursor **pp_cursor = NULL;
    if (!enif_get_resource(env, argv[0], UnqliteResourceType, (void**)&pp_cursor)) { 
	    return enif_make_badarg(env);
    }
    int rc = unqlite_kv_cursor_prev_entry(*pp_cursor);
	if( rc != UNQLITE_OK ){
        return make_fatal_tuple_info(pDb, 0, env);
	}
   return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
unqlite_cursor_entry(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){
    struct unqlite_kv_cursor **pp_cursor = NULL;
    if (!enif_get_resource(env, argv[0], UnqliteResourceType, (void**)&pp_cursor)) { 
	    return enif_make_badarg(env);
    }

    unqlite_int64 value_len;  //Data length
    int key_len;
    int rc = unqlite_kv_cursor_key(*pp_cursor, NULL, &key_len);
	if( rc != UNQLITE_OK ){
        return make_fatal_tuple_info(pDb, 0, env);
	}
    ErlNifBinary key_bin;
    if (!enif_alloc_binary(key_len, &key_bin)) { 
	return enif_make_badarg(env);
    }
    unqlite_kv_cursor_key(*pp_cursor, key_bin.data, &key_len);

    rc = unqlite_kv_cursor_data(*pp_cursor, NULL, &value_len);
	if( rc != UNQLITE_OK ){
        return make_fatal_tuple_info(pDb, 0, env);
	}

    ErlNifBinary value_bin;
    if (!enif_alloc_binary(value_len, &value_bin)) { 
	return enif_make_badarg(env);
    }
    unqlite_kv_cursor_data(*pp_cursor, value_bin.data, &value_len);

    return enif_make_tuple2(env, enif_make_binary(env, &key_bin), enif_make_binary(env, &value_bin)) ;
}



static ErlNifFunc nif_funcs[] = {
	{"init", 1, unqlite_init},
    {"set", 2, unqlite_set},
    {"get", 1, unqlite_get},
    {"del", 1, unqlite_del},
    {"commit", 0, unqlite_commit1},
    {"rollback", 0, unqlite_rollback1},
    {"init_cursor", 0, unqlite_init_cursor},
    {"cursor_first_entry", 1, unqlite_cursor_first_entry},
    {"cursor_last_entry", 1, unqlite_cursor_last_entry},
    {"cursor_valid_entry", 1, unqlite_cursor_valid_entry},
    {"cursor_next_entry", 1, unqlite_cursor_next_entry},
    {"cursor_prev_entry", 1, unqlite_cursor_prev_entry},
    {"cursor_entry", 1, unqlite_cursor_entry},
};

ERL_NIF_INIT(unqlite_nif, nif_funcs, unqlite_load, NULL, NULL, unqlite_unload);

