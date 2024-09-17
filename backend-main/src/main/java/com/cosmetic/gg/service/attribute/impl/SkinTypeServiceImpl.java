package com.cosmetic.gg.service.attribute.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.utils.string.StringUtils;
import com.cosmetic.gg.entity.attribute.SkinType;
import com.cosmetic.gg.entity.product.Product;
import com.cosmetic.gg.repository.attribute.SkinTypeRepository;
import com.cosmetic.gg.repository.product.ProductRepository;
import com.cosmetic.gg.service.attribute.SkinTypeService;

@Service
public class SkinTypeServiceImpl implements SkinTypeService {

	private static final Logger log = LoggerFactory.getLogger(SkinTypeServiceImpl.class);
	
	@Autowired
	private SkinTypeRepository skinTypeRepository;
	
	@Autowired
	private ProductRepository productRepository;
	
	@Override
	public List<SkinType> search() {
		List<SkinType> result = new ArrayList<>();
		try {
			result = skinTypeRepository.findAll();
			return result;
		}catch(Exception ex) {
			log.error("Error while searching skin type", ex.getCause());
		}
		return result;
	}
	
	@Override
	public List<Error> validator(SkinType skinType){
		List<Error> errors = new ArrayList<>();
		try {
			SkinType skinTypeCheck;
			if(Boolean.FALSE.equals(StringUtils.isNullOrEmpty(skinType.getId()))) {
				skinTypeCheck = skinTypeRepository.findById(skinType.getId()).orElse(null);
				if(skinTypeCheck == null)
					errors.add(new Error().builder(ErrorCode.NOT_FOUND));
			}
		}catch(Exception ex) {
			log.error("Error while validating skin type data", ex.getCause());
			errors.add(new Error().builder(ErrorCode.EXCEPTION));
		}
		return errors;
	}
	
	@Override
	public SkinType create(SkinType skinType) {
		try {
			skinType = skinTypeRepository.save(skinType);
			if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(skinType.getId())))
		        return null;
			
			return skinType;
		}catch(Exception ex) {
			log.error("Error while creating skin type", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public SkinType update(SkinType skinType) {
		try {
			if(skinType == null)
				return null;
			skinType = skinTypeRepository.save(skinType);
			if(skinType == null)
				return null;
			return skinType;
		}catch(Exception ex) {
			log.error("Error while updating skin type", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public SkinType getById(String id) {
		try {
			SkinType skinTypeEntity = skinTypeRepository.findById(id).orElse(null);
			if(skinTypeEntity == null || skinTypeEntity.getId() == null)
				return null;
			
			return skinTypeEntity;
		}catch(Exception ex) {
			log.error("Error while getting skin type", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public Error delete(String id) {
		try {
			SkinType skinTypeEntity = skinTypeRepository.findById(id).orElse(null);
			if(skinTypeEntity == null || skinTypeEntity.getId() == null)
				return new Error().builder(ErrorCode.NOT_FOUND);
			
			Product productCheck = productRepository.findBySkinType(id);
			if(productCheck != null) {
				return new Error().builder(ErrorCode.CHILD_REFERENCE);
			}
			
			skinTypeRepository.deleteById(id);
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error("Error while deleting brand", ex.getCause());
			return new Error().builder(ErrorCode.EXCEPTION);
		}
	}
}
