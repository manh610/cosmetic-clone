package com.cosmetic.gg.common.utils.mapper;

import com.cosmetic.gg.common.utils.object.ObjectMapperUtils;
import com.fasterxml.jackson.databind.ObjectMapper;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.Assert;

@Slf4j
public class ModelMapper {

private static final ObjectMapper objectMapper = ObjectMapperUtils.defaultInstance();
	
	@SneakyThrows
	public static <D> D map(Object source, Class<D> destinationType) {
		Assert.notNull(source, "source");
		Assert.notNull(destinationType, "destinationType");
		try {
			return objectMapper.readValue(objectMapper.writeValueAsString(source), destinationType);
		}catch(Exception ex) {
			log.error(ex.getMessage());
			return null;
		}
	}
}
