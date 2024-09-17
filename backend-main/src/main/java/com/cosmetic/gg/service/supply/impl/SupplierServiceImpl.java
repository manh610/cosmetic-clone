package com.cosmetic.gg.service.supply.impl;

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
import com.cosmetic.gg.common.utils.mapper.ModelMapper;
import com.cosmetic.gg.common.utils.string.StringUtils;
import com.cosmetic.gg.entity.User;
import com.cosmetic.gg.entity.supply.Supplier;
import com.cosmetic.gg.model.UserModel;
import com.cosmetic.gg.repository.supply.SupplierRepository;
import com.cosmetic.gg.service.supply.SupplierService;

@Service
public class SupplierServiceImpl implements SupplierService{

	private static final Logger log = LoggerFactory.getLogger(SupplierServiceImpl.class);
	
	@Autowired
	private SupplierRepository supplierRepository;
	
	@Override
	public List<Supplier> search(String keyword, EStatus status) {
		try {
			List<Supplier> suppliers = supplierRepository.search(
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(keyword)) ? null : keyword,
					Objects.isNull(status) ? null : status.name());
			return suppliers;
		}catch(Exception ex) {
			log.error("Error while searching supplier", ex.getCause());
			return null;
		}
	}
	
	@Override
	public List<Error> validator(Supplier supplier) {
		List<Error> errors = new ArrayList<>();
		try {
			Supplier supplierCheck;
			if(Boolean.FALSE.equals(StringUtils.isNullOrEmpty(supplier.getId()))) {
				supplierCheck = supplierRepository.findById(supplier.getId()).orElse(null);
				if(supplierCheck == null)
					errors.add(new Error().builder(ErrorCode.NOT_FOUND));
				
				assert supplierCheck != null;
				if(!supplierCheck.getCode().equals(supplier.getCode())) {
					errors.add(new Error().builder(ErrorCode.INVALID_DATA, "Ký hiệu không được thay đổi", "Code can not be change"));
				}
				
				assert supplierCheck != null;
				if(supplierCheck.getStatus() == EStatus.DELETED)
					errors.add(new Error().builder(ErrorCode.INVALID_STATUS));
			}
			supplierCheck = supplierRepository.findByKey(supplier.getCode());
			if(supplierCheck != null && !supplierCheck.getId().equals(supplier.getId()))
				errors.add(new Error().builder(ErrorCode.EXIST_CODE));
			
			supplierCheck = supplierRepository.findByKey(supplier.getEmail());
			if(supplierCheck != null && !supplierCheck.getId().equals(supplier.getId()))
				errors.add(new Error().builder(ErrorCode.USER_EXIST_EMAIL));
			
			supplierCheck = supplierRepository.findByKey(supplier.getPhone());
			if(supplierCheck != null && !supplierCheck.getId().equals(supplier.getId()))
				errors.add(new Error().builder(ErrorCode.USER_EXIST_PHONE));
		}catch(Exception ex) {
			log.error("Error while validating user data", ex.getCause());
			errors.add(new Error().builder(ErrorCode.EXCEPTION));
		}
		return errors;
	}
	
	@Override
	public Supplier create(Supplier supplier) {
		try {
			supplier.setStatus(EStatus.ACTIVE);
			supplier = supplierRepository.save(supplier);
			if (supplier == null)
				return null;
			return supplier;
		}catch(Exception ex) {
			log.error("Error while creating supplier", ex.getCause());
			return null;
		}
	}
	
	@Override
	public Supplier update(Supplier supplier) {
		try {
			if (supplier == null)
				return null;
			supplier = supplierRepository.save(supplier);
			supplier = supplierRepository.save(supplier);
			if (supplier == null)
				return null;
			return supplier;
		}catch(Exception ex) {
			log.error("Error while updating supplier", ex.getCause());
			return null;
		}
	}
	
	@Override
	public Error delete(String id) {
		try {
			Supplier supplierEntity = supplierRepository.findById(id).orElse(null);
			if (supplierEntity == null)
				return new Error().builder(ErrorCode.NOT_FOUND);
			
			if (supplierEntity.getStatus() == EStatus.INACTIVE) {
				supplierRepository.deleteById(id);
				return new Error().builder(ErrorCode.SUCCESS);
			}
			
			supplierEntity.setStatus(EStatus.DELETED);
			Supplier supplier = supplierRepository.save(supplierEntity);
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error("Error while deleting user", ex.getCause());
			return new Error().builder(ErrorCode.EXCEPTION);
		}
	}
	
	@Override
	public Supplier getById(String id) {
		try {
			Supplier supplierEntity = supplierRepository.findById(id).orElse(null);
			if (supplierEntity == null || supplierEntity.getStatus() == EStatus.DELETED)
				return null;
			return supplierEntity;
		}catch(Exception ex) {
			log.error("Error while getting detail supplier", ex.getCause());
			return null;
		}
	}
}
