package com.cosmetic.gg.common.utils.type;

import java.lang.reflect.GenericArrayType;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

import org.springframework.stereotype.Component;

import lombok.extern.slf4j.Slf4j;

@Component
@Slf4j
public class TypeUtils {

	public static boolean isTypeOf(final Type type, final Class<?> clazz) {
	    if (type instanceof Class) {
	      return clazz.isAssignableFrom((Class<?>) type);
	    }
	    if (type instanceof ParameterizedType) {
	      final ParameterizedType parameterizedType = (ParameterizedType) type;
	      return isTypeOf(parameterizedType.getRawType(), clazz);
	    }
	   return false;
	}
	
	public static Type[] getGenericParameter(final Type type) {
	    if (type instanceof ParameterizedType) {
	      return ((ParameterizedType) type).getActualTypeArguments();
	    }
	    if (type instanceof GenericArrayType) {
	      return getGenericParameter(((GenericArrayType) type).getGenericComponentType());
	    }
	    return null;
	}
	
	public static Type getGenericParameter(final Type type, final int index) {
	    if (!(type instanceof ParameterizedType)) {
	      return null;
	    }
	    final Type[] genericParameter = getGenericParameter(type);
	    if (genericParameter == null) {
	      return null;
	    }
	    return genericParameter[index];
	}
}
