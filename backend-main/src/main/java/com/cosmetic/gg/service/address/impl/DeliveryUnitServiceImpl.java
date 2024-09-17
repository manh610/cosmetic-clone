package com.cosmetic.gg.service.address.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.cosmetic.gg.common.enums.EDeliveryType;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.utils.string.StringUtils;
import com.cosmetic.gg.entity.address.DeliveryUnit;
import com.cosmetic.gg.repository.address.DeliveryUnitRepository;
import com.cosmetic.gg.service.address.DeliveryUnitService;

@Service
public class DeliveryUnitServiceImpl implements DeliveryUnitService{
	private static final Logger log = LoggerFactory.getLogger(DeliveryUnitServiceImpl.class);
	
	@Autowired
	private DeliveryUnitRepository deliveryUnitRepository;
	
	@Override
	public List<DeliveryUnit> search(String keyword, EDeliveryType deliveryType) {
		List<DeliveryUnit> result = new ArrayList<>();
		try {
			result = deliveryUnitRepository.search(
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(keyword)) ? null : keyword,
					Objects.isNull(deliveryType) ? null : deliveryType.name());
			return result;
		}catch(Exception ex) {
			log.error("Error while searching delivery unit", ex.getCause());
		    return result;
		}
	}
	
	@Override
	public List<Error> validator(DeliveryUnit deliveryUnit){
		List<Error> errors = new ArrayList<>();
		try {
			DeliveryUnit deliveryUnitCheck;
			if(Boolean.FALSE.equals(StringUtils.isNullOrEmpty(deliveryUnit.getId()))) {
				deliveryUnitCheck = deliveryUnitRepository.findById(deliveryUnit.getId()).orElse(null);
				if(deliveryUnitCheck == null)
					errors.add(new Error().builder(ErrorCode.NOT_FOUND));
				
				assert deliveryUnitCheck != null;
				if(!deliveryUnitCheck.getCode().equals(deliveryUnit.getCode()))
					errors.add(new Error().builder(ErrorCode.INVALID_DATA, "Ký hiệu không được thay đổi", "Code can not be change"));
			}
			
			deliveryUnitCheck = deliveryUnitRepository.findByKey(deliveryUnit.getCode());
			if(deliveryUnitCheck != null && !deliveryUnitCheck.getId().equals(deliveryUnitCheck.getId()))
				errors.add(new Error().builder(ErrorCode.EXIST_CODE));
			
			deliveryUnitCheck = deliveryUnitRepository.findByKey(deliveryUnit.getEmail());
			if(deliveryUnitCheck != null && !deliveryUnitCheck.getId().equals(deliveryUnit.getId()))
				errors.add(new Error().builder(ErrorCode.USER_EXIST_EMAIL));
			
			deliveryUnitCheck = deliveryUnitRepository.findByKey(deliveryUnit.getPhone());
			if(deliveryUnitCheck != null && !deliveryUnitCheck.getId().equals(deliveryUnit.getId()))
				errors.add(new Error().builder(ErrorCode.USER_EXIST_PHONE));
		}catch(Exception ex) {
			log.error("Error while validating delivery unit data", ex.getCause());
			errors.add(new Error().builder(ErrorCode.EXCEPTION));
		}
		return errors;
	}
	
	@Override
	public DeliveryUnit create(DeliveryUnit deliveryUnit) {
		try {
			deliveryUnit = deliveryUnitRepository.save(deliveryUnit);
			if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(deliveryUnit.getId())))
		        return null;
			
			return deliveryUnit;
		}catch(Exception ex) {
			log.error("Error while creating delivery unit", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public DeliveryUnit update(DeliveryUnit deliveryUnit) {
		try {
			if(deliveryUnit == null)
				return null;
			deliveryUnit = deliveryUnitRepository.save(deliveryUnit);
			if(deliveryUnit == null)
				return null;
			return deliveryUnit;
		}catch(Exception ex) {
			log.error("Error while updating delivery unit", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public Error delete(String id) {
		try {
			DeliveryUnit deliveryUnit = deliveryUnitRepository.findById(id).orElse(null);
			if(deliveryUnit == null)
				return new Error().builder(ErrorCode.NOT_FOUND);
			
			deliveryUnitRepository.deleteById(id);
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error("Error while deleting brand", ex.getCause());
			return new Error().builder(ErrorCode.EXCEPTION);
		}
	}
}
