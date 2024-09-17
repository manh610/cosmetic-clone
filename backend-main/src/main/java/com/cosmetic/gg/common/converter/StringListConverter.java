package com.cosmetic.gg.common.converter;

import java.util.Arrays;
import java.util.List;
import static java.util.Collections.emptyList;

import javax.persistence.AttributeConverter;

import com.cosmetic.gg.common.utils.string.StringUtils;

public class StringListConverter implements AttributeConverter<List<String>, String>{

	private static final String SPLIT_CHAR = ";";

	@Override
	public String convertToDatabaseColumn(List<String> stringList) {
		return (stringList != null && !stringList.isEmpty()) ? String.join(SPLIT_CHAR, stringList) : null;
	}

	@Override
	public List<String> convertToEntityAttribute(String string) {
		return !StringUtils.isNullOrEmpty(string) ? Arrays.asList(string.split(SPLIT_CHAR)) : emptyList();
	}
}
