package com.cosmetic.gg.common.utils.collection;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.springframework.stereotype.Component;

import com.cosmetic.gg.common.utils.type.TypeUtils;

import lombok.extern.slf4j.Slf4j;

@Component
@Slf4j
public class ListUtils {

	public static <T,R> List<R> map(List<T> list, Function<? super T, ? extends R> mapper){
	    return list.stream().map(mapper).collect(Collectors.toList());
	  }
	
	public static Type getElementTypeOfList(final Type type) {
	    if (!TypeUtils.isTypeOf(type, List.class)) {
	      return null;
	    }
	    return TypeUtils.getGenericParameter(type, 0);
	}
	
	@SafeVarargs
	public static<T> List<T> concatenate(List<T>... lists) {
	    List<T> result = new ArrayList<>();
	    Stream.of(lists).forEach(result::addAll);

	    return result;
	}
	
	@SafeVarargs
	public static<T> List<T> manySelect(List<T>... lists) {
	    return Stream.of(lists)
	        .flatMap(Collection::stream)
	        .collect(Collectors.toList());
	}
	
	public static<T> List<T> concatenate(Collection<List<T>> lists) {
	    List<T> result = new ArrayList<>();
	    lists.forEach(result::addAll);

	    return result;
	}
	
	public static<T> List<T> manySelect(Collection<List<T>> lists) {
	    return lists.stream()
	        .flatMap(Collection::stream)
	        .collect(Collectors.toList());
	}
	
	public static<T> T first(List<T> objects) {
	    return objects == null || objects.isEmpty() ? null : objects.get(0);
	  }
}
