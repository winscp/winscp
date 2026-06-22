#include "putty.h"

#define CONF_ENUM(name, ...)                                            \
    static const ConfSaveEnumValue conf_enum_values_##name[] = {        \
        __VA_ARGS__                                                     \
    }; const ConfSaveEnumType conf_enum_##name = {                      \
        .values = conf_enum_values_##name,                              \
        .nvalues = lenof(conf_enum_values_##name),                      \
    };

#define VALUE(eval, sval) { eval, sval, false }
#define VALUE_OBSOLETE(eval, sval) { eval, sval, true }

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wmissing-variable-declarations" // WINSCP

#include "conf-enums.h"

#pragma clang diagnostic pop

bool conf_enum_map_to_storage(const ConfSaveEnumType *etype,
                              int confval, int *storageval_out)
{
    for (size_t i = 0; i < etype->nvalues; i++)
        if (!etype->values[i].obsolete &&
            etype->values[i].confval == confval) {
            *storageval_out = etype->values[i].storageval;
            return true;
        }
    return false;
}

bool conf_enum_map_from_storage(const ConfSaveEnumType *etype,
                                int storageval, int *confval_out)
{
    for (size_t i = 0; i < etype->nvalues; i++)
        if (etype->values[i].storageval == storageval) {
            *confval_out = etype->values[i].confval;
            return true;
        }
    return false;
}

#define CONF_OPTION(id, ...) { __VA_ARGS__ },
#define VALUE_TYPE(x) .value_type = CONF_TYPE_ ## x
#define SUBKEY_TYPE(x) .subkey_type = CONF_TYPE_ ## x
#define DEFAULT_INT(x) .default_value.ival = x
#define DEFAULT_STR(x) .default_value.sval = x
#define DEFAULT_BOOL(x) .default_value.bval = x
#define SAVE_KEYWORD(x) .save_keyword = x
#define STORAGE_ENUM(x) .storage_enum = &conf_enum_ ## x
#define SAVE_CUSTOM .save_custom = true
#define LOAD_CUSTOM .load_custom = true
#define NOT_SAVED .not_saved = true

const ConfKeyInfo conf_key_info[] = {
    #include "conf.h"
};
