package com.cosmetic.gg.service.product.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.utils.mapper.ModelMapper;
import com.cosmetic.gg.common.utils.string.StringUtils;
import com.cosmetic.gg.entity.product.Brand;
import com.cosmetic.gg.entity.product.Product;
import com.cosmetic.gg.model.product.BrandModel;
import com.cosmetic.gg.repository.product.BrandRepository;
import com.cosmetic.gg.repository.product.ProductRepository;
import com.cosmetic.gg.service.product.BrandService;

@Service
public class BrandServiceImpl implements BrandService{

	private static final Logger log = LoggerFactory.getLogger(BrandServiceImpl.class);
	
	@Autowired
	private BrandRepository brandRepository;
	
	@Autowired
	private ProductRepository productRepository;
	
	@Override
	public List<Brand> search(String keyword, EStatus status) {
		List<Brand> result = new ArrayList<>();
		try {
			result = brandRepository.search(
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(keyword)) ? null : keyword,
					Objects.isNull(status) ? null : status.name());
			return result;
		}catch(Exception ex) {
			log.error("Error while searching brand", ex.getCause());
		}
		return result;
	}
	
	@Override
	public Brand getById(String id) {
		try {
			Brand brandEntity = brandRepository.findById(id).orElse(null);
			if(brandEntity == null)
				brandEntity = null;
			
			return brandEntity;
		}catch(Exception ex) {
			log.error(String.format("Error while getting brand by id: %s", id), ex.getCause());
			return null;
		}
	}
	
	@Override
	public List<Error> validator(BrandModel brandModel){
		List<Error> errors = new ArrayList<>();
		try {
			Brand brandCheck;
			if(Boolean.FALSE.equals(StringUtils.isNullOrEmpty(brandModel.getId()))) {
				brandCheck = brandRepository.findById(brandModel.getId()).orElse(null);
				if(brandCheck == null)
					errors.add(new Error().builder(ErrorCode.NOT_FOUND));
				
				assert brandCheck != null;
				if(!brandCheck.getCode().equals(brandModel.getCode()))
					errors.add(new Error().builder(ErrorCode.INVALID_DATA, "Ký hiệu không được thay đổi", "Code can not be change"));
				
				if(brandCheck.getStatus() == EStatus.DELETED)
					errors.add(new Error().builder(ErrorCode.INVALID_STATUS));
			}
			
			brandCheck = brandRepository.findByKey(brandModel.getCode());
			if(brandCheck != null && !brandCheck.getId().equals(brandModel.getId()))
				errors.add(new Error().builder(ErrorCode.EXIST_CODE));
		}catch(Exception ex) {
			log.error("Error while validating brand data", ex.getCause());
			errors.add(new Error().builder(ErrorCode.EXCEPTION));
		}
		return errors;
	}
	
	@Override
	public Brand create(BrandModel brandModel) {
		try {
			Brand brandEntity = ModelMapper.map(brandModel, Brand.class);
			brandEntity.prepareEntity();
			brandEntity = brandRepository.save(brandEntity);
			if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(brandEntity.getId())))
		        return null;
			
			return brandEntity;
		}catch(Exception ex) {
			log.error("Error while creating brand", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public Brand update(BrandModel brandModel) {
		try {
			Brand brandEntity = ModelMapper.map(brandModel, Brand.class);
			if(brandEntity == null)
				return null;
			brandEntity.prepareEntity();
			brandEntity = brandRepository.save(brandEntity);
			if(brandEntity == null)
				return null;
			return brandEntity;
		}catch(Exception ex) {
			log.error("Error while updating brand", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public Error delete(String id) {
		try {
			Brand brandEntity = getById(id);
			if(brandEntity == null || brandEntity.getStatus() == EStatus.DELETED)
				return new Error().builder(ErrorCode.NOT_FOUND);
			
			Product productCheck = productRepository.findByAttribute(id);
			if(productCheck != null) {
				return new Error().builder(ErrorCode.CHILD_REFERENCE);
			}
			
			brandEntity.setStatus(EStatus.DELETED);
			brandEntity = brandRepository.save(brandEntity);
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error("Error while deleting brand", ex.getCause());
			return new Error().builder(ErrorCode.EXCEPTION);
		}
	}
	
	@Override
	public ErrorCode changeImage(MultipartFile imageFile, String id) {
		try {
			Brand brandEntity = getById(id);
			if (brandEntity == null)
		        return ErrorCode.NOT_FOUND;
			
			if (brandEntity.getStatus() == EStatus.DELETED)
		    	return ErrorCode.INVALID_STATUS;
			
			String originalFilename = imageFile.getOriginalFilename();
	        String extension = originalFilename.substring(originalFilename.lastIndexOf(".") + 1);
	        if (!StringUtils.isNullOrEmpty(originalFilename) && originalFilename.length() > 0) {
	        	if (!extension.equals("png") && !extension.equals("jpg") && !extension.equals("gif") 
	        			&& !extension.equals("svg") && !extension.equals("jpeg"))
	        		return ErrorCode.INVALID_IMAGE;
	        }
			
			byte[] img = imageFile.getBytes();
			if(img == null) {
				log.error("Error while convert image");
				return ErrorCode.ERROR_CONVERT_IMAGE;
			}
			
			brandEntity.setLogo(img);
			brandEntity = brandRepository.save(brandEntity);
			if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(brandEntity.getId())))
				return ErrorCode.FAILURE;
			return ErrorCode.SUCCESS;
		}catch(Exception ex) {
			log.error("Error while changing image logo", ex.getCause());
			return ErrorCode.EXCEPTION;
		}
	}
}
