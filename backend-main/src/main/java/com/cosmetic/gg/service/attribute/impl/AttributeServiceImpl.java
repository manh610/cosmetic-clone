package com.cosmetic.gg.service.attribute.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.utils.string.StringUtils;
import com.cosmetic.gg.entity.attribute.Attribute;
import com.cosmetic.gg.entity.attribute.SkinType;
import com.cosmetic.gg.repository.attribute.AttributeRepository;
import com.cosmetic.gg.service.attribute.AttributeService;

@Service
public class AttributeServiceImpl implements AttributeService{
	
	private static final Logger log = LoggerFactory.getLogger(AttributeServiceImpl.class);
	
	@Autowired
	private AttributeRepository attributeRepository;
	
	@Override
	public List<Attribute> search(EStatus status) {
		List<Attribute> result = new ArrayList<>();
		try {
			result = attributeRepository.search(Objects.isNull(status) ? null : status.name());
			return result;
		}catch(Exception ex) {
			log.error("Error while searching attribute", ex.getCause());
		}
		return result;
	}
	
	@Override
	public List<Error> validator(Attribute attribute){
		List<Error> errors = new ArrayList<>();
		try {
			Attribute attributeCheck;
			if(Boolean.FALSE.equals(StringUtils.isNullOrEmpty(attribute.getId()))) {
				attributeCheck = attributeRepository.findById(attribute.getId()).orElse(null);
				if(attributeCheck == null)
					errors.add(new Error().builder(ErrorCode.NOT_FOUND));
				
				assert attributeCheck != null;
				if(!attributeCheck.getCode().equals(attribute.getCode())) {
					errors.add(new Error().builder(ErrorCode.INVALID_DATA, "Ký hiệu không được thay đổi", "Code can not be change"));
				}
				
				if(attributeCheck.getStatus() == EStatus.DELETED)
					errors.add(new Error().builder(ErrorCode.INVALID_STATUS));
			}
			
			attributeCheck = attributeRepository.findByKey(attribute.getCode());
			if(attributeCheck != null && !attributeCheck.getId().equals(attribute.getId()))
				errors.add(new Error().builder(ErrorCode.EXIST_CODE));
		}catch(Exception ex) {
			log.error("Error while validating attribute data", ex.getCause());
			errors.add(new Error().builder(ErrorCode.EXCEPTION));
		}
		return errors;
	}
	
	@Override
	public Attribute create(Attribute attribute) {
		try {
			attribute.setStatus(EStatus.ACTIVE);
			attribute = attributeRepository.save(attribute);
			if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(attribute.getId())))
		        return null;
			
			return attribute;
		}catch(Exception ex) {
			log.error("Error while creating attribute", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public Attribute update(Attribute attribute) {
		try {
			if(attribute == null)
				return null;
			attribute = attributeRepository.save(attribute);
			if(attribute == null)
				return null;
			return attribute;
		}catch(Exception ex) {
			log.error("Error while updating attribute", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public Error delete(String id) {
		try {
			Attribute attribute = attributeRepository.findById(id).orElse(null);
			if(attribute == null || attribute.getId() == null || attribute.getStatus() == EStatus.DELETED)
				return new Error().builder(ErrorCode.NOT_FOUND);
			
			attribute.setStatus(EStatus.DELETED);
			attribute = attributeRepository.save(attribute);
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error("Error while deleting brand", ex.getCause());
			return new Error().builder(ErrorCode.EXCEPTION);
		}
	}
	
	@Override
	public Attribute getById(String id) {
		try {
			Attribute attribute = attributeRepository.findById(id).orElse(null);
			if(attribute == null || attribute.getId() == null || attribute.getStatus() == EStatus.DELETED)
				return null;
			
			Attribute result = attributeRepository.findById(id).orElse(null);
			if(result == null) return null;
			
			return result;
		}catch(Exception ex) {
			log.error("Error while getting detail attribute", ex.getCause());
			return null;
		}
	}
}
