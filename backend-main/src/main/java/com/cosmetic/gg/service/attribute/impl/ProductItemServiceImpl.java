package com.cosmetic.gg.service.attribute.impl;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.utils.string.StringUtils;
import com.cosmetic.gg.entity.attribute.ProductItem;
import com.cosmetic.gg.entity.attribute.ValueDetail;
import com.cosmetic.gg.entity.product.Product;
import com.cosmetic.gg.repository.attribute.ProductItemRepository;
import com.cosmetic.gg.repository.attribute.ValueDetailRepository;
import com.cosmetic.gg.repository.product.ProductRepository;
import com.cosmetic.gg.service.attribute.ProductItemService;

@Service
public class ProductItemServiceImpl implements ProductItemService{

	private static final Logger log = LoggerFactory.getLogger(ProductItemServiceImpl.class);
	
	@Autowired
	private ProductItemRepository productItemRepository;
	
	@Autowired
	private ProductRepository productRepository;
	
	@Autowired
	private ValueDetailRepository valueDetailRepository;
	
	@Override
	public List<Error> validator(ProductItem productItem){
		List<Error> errors = new ArrayList<>();
		try {
			Product product = productRepository.findById(productItem.getProductId()).orElse(null);
		    if (product != null && !product.getId().equals(productItem.getProductId()))
		        errors.add(new Error().builder(ErrorCode.INVALID_PRODUCT));
		      
	      ValueDetail valueDetail = valueDetailRepository.findById(productItem.getValueDetailId()).orElse(null);
	      if (valueDetail != null && !valueDetail.getId().equals(productItem.getValueDetailId()))
	        errors.add(new Error().builder(ErrorCode.PRODUCT_INVALID_VALUE_DETAIL));
		}catch(Exception ex) {
			log.error("Error while validating product item data", ex.getCause());
			errors.add(new Error().builder(ErrorCode.EXCEPTION));
		}
		return errors;
	}
	
	@Override
	public ProductItem create(ProductItem productItem) {
		try {
			productItem = productItemRepository.save(productItem);
			if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(productItem.getId())))
		        return null;
			
			return productItem;
		}catch(Exception ex) {
			log.error("Error while creating value detail", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public Error delete(String id) {
		try {
			ProductItem productItem = productItemRepository.findById(id).orElse(null);
			if(productItem == null || productItem.getId() == null)
				return new Error().builder(ErrorCode.NOT_FOUND);
			
			ValueDetail valueDetail = valueDetailRepository.getByProductItem(id);
			if(valueDetail == null || valueDetail.getId() == null)
				return new Error().builder(ErrorCode.NOT_FOUND);
			
			productItemRepository.deleteById(productItem.getId());
			valueDetailRepository.deleteById(valueDetail.getId());
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error("Error while deleting brand", ex.getCause());
			return new Error().builder(ErrorCode.EXCEPTION);
		}
	}
}
