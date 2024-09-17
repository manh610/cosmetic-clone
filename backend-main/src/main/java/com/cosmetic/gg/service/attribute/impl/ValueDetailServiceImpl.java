package com.cosmetic.gg.service.attribute.impl;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.utils.mapper.ModelMapper;
import com.cosmetic.gg.common.utils.string.StringUtils;
import com.cosmetic.gg.dto.response.attribute.ValueDetailResponse;
import com.cosmetic.gg.entity.attribute.Attribute;
import com.cosmetic.gg.entity.attribute.ProductItem;
import com.cosmetic.gg.entity.attribute.ValueDetail;
import com.cosmetic.gg.entity.product.Product;
import com.cosmetic.gg.model.attribute.ValueDetailModel;
import com.cosmetic.gg.repository.attribute.ProductItemRepository;
import com.cosmetic.gg.repository.attribute.ValueDetailRepository;
import com.cosmetic.gg.repository.product.ProductRepository;
import com.cosmetic.gg.service.attribute.ValueDetailService;

@Service
public class ValueDetailServiceImpl implements ValueDetailService{

	private static final Logger log = LoggerFactory.getLogger(ValueDetailServiceImpl.class);
	
	@Autowired
	private ValueDetailRepository valueDetailRepository;
	
	@Autowired
	private ProductRepository productRepository;
	
	@Autowired
	private ProductItemRepository productItemRepository;
	
	@Override
	public List<Error> validator(ValueDetailModel valueDetailModel){
		List<Error> errors = new ArrayList<>();
		try {
			ValueDetail valueDetailCheck;
			if(Boolean.FALSE.equals(StringUtils.isNullOrEmpty(valueDetailModel.getId()))) {
				valueDetailCheck = valueDetailRepository.findById(valueDetailModel.getId()).orElse(null);
				if(valueDetailCheck == null)
					errors.add(new Error().builder(ErrorCode.NOT_FOUND));
			}
			Product productCheck = productRepository.findById(valueDetailModel.getProductId()).orElse(null);
			if(productCheck == null)
				errors.add(new Error().builder(ErrorCode.NOT_FOUND));
		}catch(Exception ex) {
			log.error("Error while validating value detail data", ex.getCause());
			errors.add(new Error().builder(ErrorCode.EXCEPTION));
		}
		return errors;
	}
	
	@Override
	public ValueDetail create(ValueDetailModel valueDetailModel) {
		try {
			ValueDetail valueDetail = ModelMapper.map(valueDetailModel, ValueDetail.class);
			valueDetail = valueDetailRepository.save(valueDetail);
			if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(valueDetail.getId())))
		        return null;
			ProductItem productItem = new ProductItem();
			productItem.setValue(valueDetailModel.getValue());
			productItem.setProductId(valueDetailModel.getProductId());
			productItem.setValueDetailId(valueDetail.getId());
			productItem = productItemRepository.save(productItem);
			if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(productItem.getId())))
		        return null;
			return valueDetail;
		}catch(Exception ex) {
			log.error("Error while creating value detail", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public ValueDetail update(ValueDetailModel valueDetailModel) {
		try {
			ValueDetail valueDetail = ModelMapper.map(valueDetailModel, ValueDetail.class);
			if(valueDetail == null)
				return null;
			valueDetail = valueDetailRepository.save(valueDetail);
			if(valueDetail == null)
				return null;
			return valueDetail;
		}catch(Exception ex) {
			log.error("Error while updating value detail", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public ValueDetailModel detail(String id) {
		try {
			ValueDetail valueCheck = valueDetailRepository.findById(id).orElse(null);
			if(valueCheck == null || valueCheck.getId() == null)
				return null;
			
			Object rs = valueDetailRepository.detail(id);
			if(rs instanceof Object[]) {
				Object[] objArray = (Object[]) rs;
				ValueDetailModel valueResponse = new ValueDetailModel();
				valueResponse.setId((String) objArray[0]);
				valueResponse.setImportPrice((Float) objArray[1]);
				valueResponse.setSellPrice((Float) objArray[2]);
				valueResponse.setImportQuantity((Integer) objArray[3]);
				valueResponse.setSellQuantity((Integer) objArray[4]);
				
				EStatus status = ((String) objArray[5]).equalsIgnoreCase("STOCK") ? EStatus.STOCK : EStatus.SOLD_OUT;
				valueResponse.setStatus(status);
				valueResponse.setUnit((String) objArray[6]);
				valueResponse.setDescription((String) objArray[7]);
				valueResponse.setImage((byte[]) objArray[8]);
				valueResponse.setValue((String) objArray[9]);
				valueResponse.setProductId((String) objArray[10]);
				
				return valueResponse;
			}
			return null;
		}catch(Exception ex) {
			log.error("Error while getting detail value detail", ex.getCause());
		    return null;
		}
	}
}
